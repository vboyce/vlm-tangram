# let's try reading them in from github? since they're in another repo

# based on https://github.com/vboyce/multiparty-tangrams/blob/main/code/prep_ms.R

url <- "https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/"
one_chat <- read_csv(str_c(url, "data/study1/filtered_chat.csv")) |> mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_chat <- read_csv(str_c(url, "data/study2a/filtered_chat.csv")) |> mutate(rotate = "no_rotate")
two_b_chat <- read_csv(str_c(url, "data/study2b/filtered_chat.csv")) |>
  mutate(rotate = "full_feedback") |>
  select(-`row num`)
two_c_chat <- read_csv(str_c(url, "data/study2c/filtered_chat.csv")) |>
  mutate(rotate = "emoji") |>
  select(-type)
three_chat <- read_csv(str_c(url, "data/study3/filtered_chat.csv")) |>
  inner_join(read_rds(str_c(url, "data/study3/round_results.rds")) |> select(gameId, trialNum, condition = name) |> unique()) |>
  select(-rowid, -type)

combined_chat <- one_chat |>
  rbind(two_a_chat) |>
  rbind(two_b_chat) |>
  rbind(two_c_chat) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  rbind(three_chat) |>
  filter(!(is.chitchat)) |>
  mutate(
    text = gsub("\\n", "", fixed = T, spellchecked), # note that this is using spellcorrected version!!!!
    text = gsub("[/?/.]", " ", text),
    text = str_squish(text),
    tangram = gsub("/experiment/tangram_", "", target, fixed = TRUE),
    tangram = gsub(".png", "", tangram, fixed = TRUE),
    words=str_count(text, "\\S+")
  ) %>%
  filter(role=="speaker") |> 
  group_by(gameId, trialNum, repNum, tangram, playerId, numPlayers, condition) |> 
  summarize(words=sum(words)) |> 
  select(gameId, trialNum, repNum, tangram, playerId, numPlayers, condition, words) 
# so here we instead count non-white space chunks for words

labels <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L") |> map(~ str_c("<img src=", here(images, str_c("tangram_", ., ".png")), " width='20'/>"))

label <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

color_scheme <- c(
  "2_rotate" = "#FFBDD4", "5_rotate" = "#A12EFF", "3_rotate" = "#FF7DF0", "6_rotate" = "#6940FF", "4_rotate" = "#D24AFF", "full_feedback" = "#425df5", "no_rotate" = "#00A2FF", "emoji" = "#D47E04", "2_thin" = "#FFDA09", "6_thin" = "#D47E04",
  "2_thick" = "#77F3DB", "6_thick" = "#00BDA8"
)

plot_accuracy <- function(model, title) {
  model |>
    left_join(combined_chat) |>
    mutate(correct = prediction == label) |>
    mutate(expt = case_when(
      condition %in% c("emoji", "full_feedback", "no_rotate") ~ 2,
      str_detect(condition, "rotate") ~ 1,
      T ~ 3
    )) |>
    group_by(condition, repNum, expt) |>
    summarize(model_correct = mean(correct)) |>
    ggplot(aes(x = repNum, y = model_correct, color = condition)) +
    geom_point() +
    geom_line() +
    facet_wrap(~expt) +
    scale_color_manual(values = color_scheme) +
    coord_cartesian(ylim = c(0, 1), expand = F) +
    geom_hline(yintercept = 1 / 12, linetype = "dotted") +
    labs(title = title)
}

plot_accuracy_tangram <- function(model, title) {
  by_tangram <- model |>
    mutate(correct = prediction == label) |>
    group_by(label, repNum) |>
    summarize(model_correct = mean(correct))
  
  foo <- tibble(label, labels) |>
    left_join(by_tangram) |>
    group_by(label, labels) |>
    summarize(m = mean(model_correct)) |>
    arrange(m)
  
  by_tangram |> ggplot(aes(x = reorder(label, model_correct), y = model_correct, color = as.factor(repNum))) +
    geom_point() +
    scale_color_viridis(discrete = T) +
    coord_cartesian(ylim = c(0, 1)) +
    geom_hline(yintercept = 1 / 12, linetype = "dotted") +
    scale_x_discrete(name = NULL, labels = foo$labels) +
    theme(axis.text.x = element_markdown(color = "black", size = 11)) +
    labs(title = title)
}

plot_accuracy_length <- function(model, title){
  model |>
    left_join(combined_chat) |>
    mutate(correct = ifelse(prediction == label, 1, 0)) |>
    ggplot(aes(x = words, y = correct, color = as.factor(repNum))) +
    geom_smooth(method = "lm", formula = y ~ log(x)) +
    scale_color_viridis(discrete = T) +
    coord_cartesian(ylim = c(0, 1))+
    labs(color="Round", title=title)
}

do_confusion <- function(model, title) {
  confusion <- model |>
    group_by(label, prediction) |>
    tally() |>
    group_by(label) |>
    mutate(pct = n / sum(n))
  
  self <- confusion |>
    filter(label == prediction) |>
    select(label, self = pct)
  
  corr_order <- tibble(label, labels) |>
    left_join(self) |>
    arrange(self)
  
  self_2 <- confusion |>
    ungroup() |>
    filter(label == prediction) |>
    select(prediction, self_2 = pct)
  
  
  confusion |>
    left_join(self) |>
    left_join(self_2) |>
    ggplot(aes(x = reorder(label, self, FUN = mean), y = reorder(prediction, self_2, FUN = mean), fill = pct)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_x_discrete(name = "Correct", labels = corr_order$labels) +
    scale_y_discrete(name = "Model label", labels = corr_order$labels) +
    theme(axis.text = element_markdown(color = "black", size = 11)) +
    labs(title = title)
}

human_order <- human |>
  rename(tangram = correct_tangram) |>
  group_by(tangram) |>
  summarize(correct = mean(correct)) |>
  arrange(correct)

foo <- tibble(tangram = label, labels) |>
  left_join(human_order) |>
  arrange(correct)

tangram_levels <- human_order$tangram

plot_model_naive_human <- function(model, title) {
  d_subset <- model |>
    left_join(combined_chat) |>
    filter(repNum %in% c(0, 5)) |>
    filter(condition %in% c("2_rotate", "6_rotate", "2_thin", "6_thin", "2_thick", "6_thick"))
  
  human_summary <- human |>
    group_by(tangram = correct_tangram, condition, round) |>
    summarize(correct = mean(correct)) |>
    mutate(source = "human")
  
  model_summary <- d_subset |>
    mutate(round = str_c("round_", as.character(repNum + 1))) |>
    mutate(correct = tangram == prediction) |>
    group_by(tangram, condition, round) |>
    summarize(correct = mean(correct)) |>
    mutate(source = "model")
  
  both <- human_summary |>
    bind_rows(model_summary) |>
    mutate(tangram = factor(tangram, levels = tangram_levels))
  
  one <- ggplot(both, aes(x = tangram, y = correct, group = source, color = source)) +
    geom_point(position = position_dodge(width = .3)) +
    scale_x_discrete(name = NULL, labels = foo$labels) +
    geom_hline(yintercept = 1 / 12, linetype = "dotted") +
    theme(axis.text = element_markdown(color = "black", size = 11)) +
    theme(legend.position = "bottom") +
    labs(title = title)
  
  both_wide <- human_summary |>
    select(-source) |>
    rename(Human = correct) |>
    left_join(model_summary |> select(-source) |> rename(Model = correct))
  
  two <- both_wide |>
    ggplot(aes(x = Human, y = Model, color = tangram)) +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    geom_abline() +
    theme(legend.position = "none")
  
  list(plot_grid(one, two, rel_widths = c(1.5, 1)), str_c("Correlation between ", title, " model and human ", round(cor(both_wide$Human, both_wide$Model), 3)))
}


plot_individual_corr <- function(model,title){
  d_subset <- model |>
    left_join(combined_chat) |>
    filter(repNum %in% c(0, 5)) |>
    filter(condition %in% c("2_rotate", "6_rotate", "2_thin", "6_thin", "2_thick", "6_thick")) |>
    mutate(round = str_c("round_", as.character(repNum + 1))) |>
    mutate(model_correct = ifelse(tangram == prediction, 1, 0)) |>
    select(gameId, prediction, tangram, condition, round, model_correct)
  
  
  
  
  for_corr <- human |>
    select(gameId, selected, human_correct = correct, tangram = correct_tangram, condition, round, group_size, thickness) |>
    left_join(d_subset) |>
    filter(!is.na(prediction))
  
  
  model_hum_confusion <- for_corr |>
    group_by(selected, prediction) |>
    tally() |>
    group_by(selected) |>
    mutate(pct = n / sum(n))
  
  self <- model_hum_confusion |>
    filter(selected == prediction) |>
    select(selected, self = pct)
  
  corr_order <- tibble(label, labels) |>
    left_join(self |> rename(label = selected)) |>
    arrange(self)
  
  self_2 <- model_hum_confusion |>
    ungroup() |>
    filter(selected == prediction) |>
    select(prediction, self_2 = pct)
  
  
  model_hum_confusion_plot <- model_hum_confusion |>
    left_join(self) |>
    left_join(self_2) |>
    ggplot(aes(x = reorder(selected, self, FUN = mean), y = reorder(prediction, self_2, FUN = mean), fill = pct)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue") +
    scale_x_discrete(name = "Human label", labels = corr_order$labels) +
    scale_y_discrete(name = "Model label", labels = corr_order$labels) +
    theme(axis.text = element_markdown(color = "black", size = 11))+
    labs(title=title)
  
  summ <- for_corr |>
    group_by(group_size, thickness, round, tangram) |>
    summarize(human_correct = mean(human_correct), model_correct = mean(model_correct)) |>
    mutate(diff = human_correct - model_correct) |>
    mutate(image = str_c("tangram_", tangram, ".png"))
  
  
  model_performance <- ggplot(summ, aes(x = str_c(group_size, "\n", thickness), y = diff, color = round, group = round)) +
    geom_point(position = position_dodge(width = .2)) +
    geom_hline(aes(yintercept = 0)) +
    stat_summary(fun.data = "mean_cl_boot", color = "black", position = position_dodge(width = .2)) +
    labs(y = "Human over Model accuracy", x = "Condition", title=title)
  
  list(model_hum_confusion_plot, cor.test(for_corr$human_correct, for_corr$model_correct), model_performance)
}

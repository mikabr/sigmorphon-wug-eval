# Create dumbbell plot for dev set speaker ratings
# E Chodroff
# 13 June 2021

require(tidyverse)
require(ggrepel)

### SET LANGUAGE: CHANGE ME ###
lang <- "eng" #deu, nld, eng
axislabel <- "English" #German, Dutch, English
path_to_dev_file <- "/Volumes/GoogleDrive/My Drive/sigmorphon/SharedTask0_Part2/dev/"
path_to_output <- "~/Desktop/"
###############################

# SET THEME
theme_mikabr <- function(base_size = 12, base_family = "Source Sans") {
  ggplot2::theme_bw(base_size = base_size) %+replace%
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(),
                   strip.text.x = element_text(margin = margin(b = 2)),
                   strip.text = ggplot2::element_text(face = "bold"))
}

.font <- "Source Sans Pro"
theme_set(theme_mikabr(base_family = "Source Sans"))

# IMPORT DATASET
dev <- read_csv(paste0(path_to_dev_file, lang, "_dev_orth.csv"), col_names = FALSE)
colnames(dev) <- c("lemma", "form", "tag", "rating", "orth_l", "orth_f")
dev$lemma <- gsub(" ", "", dev$lemma)
dev$form <- gsub(" ", "", dev$form)
dev$tag <- NULL
dev$human_rating <- dev$rating / 7
dev$type <- rep(c("model first choice", "model second choice"), nrow(dev)/2)

# GET SPKR RATING FOR EACH FORM ON A SINGLE ROW
dev_wide <- dev %>% select(lemma, form, human_rating, type) %>% pivot_wider(lemma, names_from = type, values_from = human_rating)

# DETERMINE WHETHER RANK OF SPKR RATING MATCHES THAT OF MODEL RANK
dev_wide$flipped <- ifelse(dev_wide$`model first choice` > dev_wide$`model second choice`, "congruent", "incongruent")
dev <- merge(dev, dev_wide, by = "lemma")

# GET LEMMA, FORM1, FORM2 ON ONE ROW
dev_wide2 <- dev %>% select(lemma, form, type) %>% pivot_wider(names_from = type, values_from = form)
colnames(dev_wide2) <- c("lemma", "first_form", "second_form")

# MERGE AWAY
dev <- merge(dev, dev_wide2, by = "lemma")

# CREATE PLOT
dumb <- ggplot(dev, aes(x = human_rating, y = orth_l)) + 
  geom_line(aes(group = orth_l), color = "grey") + 
  geom_point(color = "black", size = 0.2) + 
  geom_text_repel(aes(label = orth_f, color = type), size = 3, direction = "x", force = 2) +
  theme_mikabr(16) + 
  scale_y_discrete(limits = rev) + 
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), limits = c(-0.01, 1.01)) +
  ylab("lemma") + 
  xlab(paste0(axislabel, " speaker ratings")) + 
  scale_color_viridis_d(begin = 0.2, end = 0.5) +
  facet_wrap(~flipped, scales = "free_x") +
  guides(color = FALSE, label = FALSE) 
ggsave(paste0(path_to_output, lang, "_dev_dumbbell.png"), dpi = 300, height = 11, width = 8, plot = dumb)

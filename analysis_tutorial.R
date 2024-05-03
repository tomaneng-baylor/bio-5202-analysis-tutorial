# start with clean environment
rm(list = ls())

library(tidyverse)
library(ggthemes)

# import relevant data copied from LibreOffice (original file causes issues)
readings <- read_tsv(
  'example_day4.txt'
  ) |> 
  pivot_longer(everything(), names_to = "well", values_to = "absorbance")

# for the sake of simplicity, ignoring drug treatment
vehicle <- filter(
  readings,
  str_detect(well, "[BDF][2-9]|[BDF]10")
  )

View(vehicle)

# Assign experimental groups
ecm <- function(wells) {
  substrate <- sub("B[0-9]+", "plastic", wells)
  substrate <- sub("D[0-9]+", "FN1", substrate)
  substrate <- sub("F[0-9]+", "COL1", substrate)
  return(substrate)
}

ko <- function(wells) {
  line <- sub("[BDF][2-4]", "sgCntrl", wells)
  line <- sub("[BDF][5-7]", "sgRAI14(2)", line)
  line <- sub("[BDF][89]|[BDF]10", "sgRAI14(3)", line)
  return(line)
}

vehicle <- vehicle |> mutate(substrate = ecm(well), line = ko(well))

anova <- aov(absorbance ~ substrate * line, vehicle)
summary(anova)
TukeyHSD(anova)

vehicle |> ggplot(
    mapping = aes(x = line, y = absorbance, fill = line)
  ) + 
  geom_bar(stat = "summary", fun = "mean") +
  facet_wrap(~substrate) +
  labs(
    x = NULL,
    y = "Absorbance",
    fill = "Cell line derivative"
  ) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
  )
ggsave(
  filename = "proliferation.png", width = 11, height=4.375
)
  

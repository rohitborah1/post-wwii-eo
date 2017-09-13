## Load requisite libraries.
library(tidyverse)
library(ggplot2)
library(reshape2)
library(readr)

## Import dataset and create dataframe.
eo_master <-
  read_csv("~/R/Post-WWII Executive Orders/eo_master.csv")
View(eo_master)
df <- as.data.frame(eo_master)

## Add df$Count, remove df$Title, and reorder dataframe.
df$Count <- 3375:1
df <- df[, c(5, 1:3)]
df <- df[order(df$Count),]

## View variable types; change as necessary.

str(df)
df$Count <- as.numeric(df$Count)
df$Year <- as.ordered(df$Year)
df$President <- as.ordered(df$President)

## Change $President to a manually ordered factor.

df$President <-
  ordered(
    df$President,
    levels = c(
      "Eisenhower",
      "Kennedy",
      "Johnson",
      "Nixon",
      "Ford",
      "Carter",
      "Reagan",
      "Bush",
      "Clinton",
      "W_Bush",
      "Obama",
      "Trump"
    )
  )

## Create second dataframe (df1) for streamlined count by president.

df1 <- data.frame(
  fNum = ordered(c(34:45)),
  President = ordered(
    c(
      "Eisenhower",
      "Kennedy",
      "Johnson",
      "Nixon",
      "Ford",
      "Carter",
      "Reagan",
      "Bush",
      "Clinton",
      "W_Bush",
      "Obama",
      "Trump"
    ),
    levels = c(
      "Eisenhower",
      "Kennedy",
      "Johnson",
      "Nixon",
      "Ford",
      "Carter",
      "Reagan",
      "Bush",
      "Clinton",
      "W_Bush",
      "Obama",
      "Trump"
    )
  ),
  Count = c(483, 214, 325, 346, 169, 320, 381, 166, 364, 290, 274, 43),
  Days = c(2922, 1036, 1886, 2027, 895, 1461, 2922, 1461, 2922, 2922, 2922, 233)
)

df1$Years = df1$Days / 365 ## Years in Office
df1$EO_365 = df1$Count / df1$Years ## EOs per 365 Days

## Plot Executive Orders by President: Vertical Bar Graph

df1 %>%
  ggplot(aes(x = President,
             y = Count,
             fill = President)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(
               angle = -45,
               hjust = 0,
               vjust = 1
             )) + labs(
               title = "Executive Orders Issued by President",
               subtitle = "Post-World War II",
               x = "President",
               y = "Count"
             ) + theme(legend.position = "none")

## Plot Executive Orders by President: Horizontal Bar Graph

## Create rPres, the reverse ordered factor for df1$President.

df1$rPres <-
  ordered(df1$President, levels = rev(levels(df1$President)))

df1 %>%
  ggplot(aes(x = rPres,
             y = Count)) + geom_bar(stat = "identity", fill = "skyblue3") + coord_flip() + labs(
               title = "Executive Orders Issued by President",
               subtitle = "After World War II",
               x = "President",
               y = "Count"
             ) + theme(legend.position = "none")

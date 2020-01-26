# Library
library(akima)
# data <- read.csv("/Users/yong/pitch/alldata.csv")
# data$px <- round(data$px, digits = 1)
# data$pz <- round(data$pz, digits = 1)

data_small_ast2016home <- data[!is.na(data$px) & !is.na(data$pz) & data$home_team == "hou" & data$g_id >= 201600001 & data$g_id < 201700001 & data$top == "True",]
data_small_ast2016away <- data[!is.na(data$px) & !is.na(data$pz) & data$away_team == "hou" & data$g_id >= 201600001 & data$g_id < 201700001 & data$top == "False",]

data_small_ast2017home <- data[!is.na(data$px) & !is.na(data$pz) & data$home_team == "hou" & data$g_id >= 201700001 & data$g_id < 201800001 & data$top == "True",]
data_small_ast2017away <- data[!is.na(data$px) & !is.na(data$pz) & data$away_team == "hou" & data$g_id >= 201700001 & data$g_id < 201800001 & data$top == "False",]

returntakenoff <- function(full_data) {
  taken <- full_data[full_data$code == "B" | full_data$code == "*B" | full_data$code == "C",]
  taken_off <- taken[taken$pitch_type != "FF" & taken$pitch_type != "FS" & taken$pitch_type != "FT" & taken$pitch_type != "FC",]
  return(taken_off)
}

returnswungoff <- function(full_data) {
  swung <- full_data[full_data$code == "S" | full_data$code == "F" | full_data$code == "X" | full_data$code == "D" | full_data$code == "E",]
  swung_off <- swung[swung$pitch_type != "FF" & swung$pitch_type != "FS" & swung$pitch_type != "FT" & swung$pitch_type != "FC",]
  return(swung_off)
}

data_small_ast2017home_taken_off <- returntakenoff(data_small_ast2017home)
data_small_ast2017home_swung_off <- returnswungoff(data_small_ast2017home)
data_small_ast2017away_taken_off <- returntakenoff(data_small_ast2017away)
data_small_ast2017away_swung_off <- returnswungoff(data_small_ast2017away)

data_small_ast2016home_taken_off <- returntakenoff(data_small_ast2016home)
data_small_ast2016home_swung_off <- returnswungoff(data_small_ast2016home)
data_small_ast2016away_taken_off <- returntakenoff(data_small_ast2016away)
data_small_ast2016away_swung_off <- returnswungoff(data_small_ast2016away)

createheatmap <- function(px, pz, year, location, decision) {
  counts <- as.data.frame.matrix(table(px, pz))
  px_list <- c(-30, 30, 30, -30)
  pz_list <- c(-30, 30, -30, 30)
  pxpz_counts <- c(0, 0, 0, 0)
  
  for (i in rownames(counts)) {
    for (j in colnames(counts)) {
      px_list <- c(px_list, round(as.numeric(i), digits = 1))
      pz_list <- c(pz_list, round(as.numeric(j), digits = 1))
      if (is.null(counts[toString(i),toString(j)])) {
        pxpz_counts <- c(pxpz_counts, 0)
      } else {
        pxpz_counts <- c(pxpz_counts, counts[toString(i),toString(j)])
      }
    }
  }
  
  pxpz_counts[is.na(pxpz_counts)] <- 0
  refined <- data.frame(px_list, pz_list, pxpz_counts)
  
  resolution <- 0.08 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
  a <- interp(x=refined$px_list, y=refined$pz_list, z=refined$pxpz_counts, 
              xo=seq(min(refined$px_list),max(refined$px_list),by=resolution), 
              yo=seq(min(refined$pz_list),max(refined$pz_list),by=resolution), duplicate="mean")

  filled.contour(a, 
                 xlim = c(-2,2),
                 ylim = c(-1,5),
                 plot.axes = {segments(c(-8.5/12,-8.5/12,8.5/12,8.5/12),c(3.4,1.3,1.3,3.4),c(-8.5/12,8.5/12,8.5/12,-8.5/12),c(1.3,1.3,3.4,3.4))},
                 plot.title = title(main = paste0("Heatmap of Pitches ", decision, " While ", location, " in ", year), xlab = "Horizontal Displacement", ylab = "Vertical Displacement", cex.main = 1),
                 key.title = title(main = "Number of Pitches", cex.main = 0.5),
                 color.palette=terrain.colors)
  # filled.contour(a, color.palette=terrain.colors)
  # image(a) #you can of course modify the color palette and the color categories. See ?image for more explanation
  #   geom_tile()
}

createheatmap(data_small_ast2017home_taken_off$px, data_small_ast2017home_taken_off$pz, 2017, "Home", "Taken")
createheatmap(data_small_ast2017away_taken_off$px, data_small_ast2017away_taken_off$pz, 2017, "Away", "Taken")

createheatmap(data_small_ast2017home_swung_off$px, data_small_ast2017home_swung_off$pz, 2017, "Home", "Swung at")
createheatmap(data_small_ast2017away_swung_off$px, data_small_ast2017away_swung_off$pz, 2017, "Away", "Swung at")

createheatmap(data_small_ast2016home_taken_off$px, data_small_ast2016home_taken_off$pz, 2016, "Home", "Taken")
createheatmap(data_small_ast2016away_taken_off$px, data_small_ast2016away_taken_off$pz, 2016, "Away", "Taken")

createheatmap(data_small_ast2016home_swung_off$px, data_small_ast2016home_swung_off$pz, 2016, "Home", "Swung at")
createheatmap(data_small_ast2016away_swung_off$px, data_small_ast2016away_swung_off$pz, 2016, "Away", "Swung at")
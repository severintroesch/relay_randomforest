#### ---------------------------------------- ####
#### SOSA 2019 Mixed team relay random forest ####
#### ---------------------------------------- ####


### prep ####

setwd ("\\\\adb.intra.admin.ch/Userhome$/BASPO-01/U80822632/config/Desktop/R/Projekte_Analysen/mixed team relay")

library(tidyverse)
library(lubridate)
library(GGally)
library(randomForest)
library(gridExtra)
library(party)
library(pheatmap)


### get and handle data ####

# filter of triathlon.org/results: https://www.triathlon.org/results#q=&hPP=15&idx=events_reverse_sort&p=0&dFR%5Bevent_categories.cat_name%5D%5B0%5D=World%20Championships&dFR%5Bevent_categories.cat_name%5D%5B1%5D=Continental%20Championship&dFR%5Bevent_categories.cat_name%5D%5B2%5D=World%20Triathlon%20Series&dFR%5Bevent_region_name%5D%5B0%5D=Europe&dFR%5Bevent_region_name%5D%5B1%5D=Oceania&dFR%5Bevent_region_name%5D%5B2%5D=Asia&dFR%5Bevent_region_name%5D%5B3%5D=Americas&dFR%5Bspecification.cat_name%5D%5B0%5D=Mixed%20Relay&dFR%5Bsport.cat_name%5D%5B0%5D=Triathlon&dFR%5Byear%5D%5B0%5D=2018&fR%5Bfederation_event%5D%5B0%5D=false&is_v=1
# ohne youth (nur U23 und elite)

d_team <- read.csv("\\\\adb.intra.admin.ch/Userhome$/BASPO-01/U80822632/config/Desktop/R/Projekte_Analysen/mixed team relay/mtr_tri_2018_teams.csv", header = T, sep = ";")
d_einz <- read.csv("\\\\adb.intra.admin.ch/Userhome$/BASPO-01/U80822632/config/Desktop/R/Projekte_Analysen/mixed team relay/mtr_tri_2018_athletes.csv", header = T, sep = ";")

str(d_team)
str(d_einz)


## clean file

# haldle times (zeit-spalten mit h:m:s-fomrat aus excel in zeit (hms) konvertieren)
new_cols_team <- d_team %>% 
  select(c(7:10,12)) %>% 
  lapply(.,function(x) {
    hms(x) %>% as.numeric() # in seconds
  }) %>% 
  data.frame() 

new_cols_einz <- d_einz %>% 
  select(c(8:13)) %>% 
  lapply(.,function(x) {
    hms(x) %>% as.numeric() # in seconds
  }) %>% 
  data.frame() 



# zeitspalten in altem df durch sekunden-spalten ersetzen 

d_team[,c(7:10,12)] <- new_cols_team
d_einz[,c(8:13)]    <- new_cols_einz


# keep only finished and classified races
d_team <- d_team %>% filter(team_tot_time != "NA")
d_team <- d_team %>% filter(team_position != "NC")
d_team[,"team_position"] <- as.numeric(paste(d_team$team_position))
d_team <- d_team %>% filter(leg_1 > 0 & leg_2 > 0 & leg_3 > 0 & leg_4 > 0) 



d_einz[,"team_position"] <- as.numeric(paste(d_einz$team_position)) #zuerst das bei einzel
d_einz <- d_einz %>% filter(team_position != "NA")
d_einz <- d_einz %>% filter(athlete_tot_time > 0)
d_einz <- d_einz %>% filter(swim > 0)
d_einz$leg <- factor(d_einz$leg)


# df mit zeiten nach leg in variablen aufgeteilg
d_aufg <- bind_cols(d_einz %>% filter(leg == 1) %>% select(1:6, 14:17),
                    d_einz %>% filter(leg == 1) %>% select(8:13),
                    d_einz %>% filter(leg == 2) %>% select(8:13),
                    d_einz %>% filter(leg == 3) %>% select(8:13),
                    d_einz %>% filter(leg == 4) %>% select(8:13)) #achtung: zeiten fuer leg2 haben suffix "1", leg3 suffix "2" usw.



### overview data ####

str(d_team)
str(d_einz)

table(d_team$nationality)

ggpairs(d_team[,7:12])
ggpairs(d_einz[,8:14])
ggpairs(d_aufg[,c(7,16,22,28,34)])



### "prediction" von Endrang des Teams ####

?randomForest

# all on team_position  - NICHT NEHMEN, DA VARIABLEN SUMME VONEINANDER?
rf1 <- randomForest(team_position ~ ., data = d_aufg %>% select(7,11:34), importance = T)
rf1
varImpPlot(rf1)

# only athlate tot times on team_position
rf2 <- randomForest(team_position ~ ., data = d_aufg %>% select(7,16,22,28,34), importance = T)
rf2
importance(rf2)
varImpPlot(rf2)

# only discipline times (not tot times) on team_position
rf3 <- randomForest(team_position ~ ., data = d_aufg %>% select(7,11:15,17:21,23:27,29:33), importance = T)
rf3
importance(rf3)[,1] %>% sqrt() %>% sort()
varImpPlot(rf3)

# only legs on team position - NICHT NEHMEN
rf4 <- randomForest(team_position ~ ., data = d_team %>% select(7:10,11), importance = T)
rf4
varImpPlot(rf4)


### PARTY RANDOM FORESTS ####
## better with "party"-random forest, 'cause data are correlated (s CAS ETH MVstat VL)
# party package
?cforest
?varimp

# only discipline times (not tot times) on team_position
crf3 <- cforest(team_position ~ ., data = d_aufg %>% select(7,11:15,17:21,23:27,29:33))
crf3_varimp <- varimp(crf3,conditional=TRUE)
barplot(crf3_varimp[order(crf3_varimp,decreasing = TRUE)][1:5])
barplot(crf3_varimp[order(crf3_varimp,decreasing = TRUE)])

varimp #varimp in regression tree seems to be given by the change in MSE (also hier rang^2) by permutation


# only athlate tot times on team_position
crf2 <- cforest(team_position ~ ., data = d_aufg %>% select(7,16,22,28,34))
crf2_varimp <- varimp(crf2,conditional=TRUE)
barplot(crf2_varimp[order(crf2_varimp,decreasing = TRUE)])

# visualize
?heatmap
mat_crf3 <- matrix(crf3_varimp, ncol = 4)

rownames(mat_crf3) <- c("Swim","Trans. 1","Bike","Trans.2","Run")
colnames(mat_crf3) <- c("Athlete 1","Athlete 2","Athlete 3","Athlete 4")

heatmap(mat_crf3, Rowv=NA, Colv=NA, scale = "none", col = rev(cm.colors(256)))
heatmap(mat_crf3, Rowv=NA, Colv=NA, scale = "none", col = colorRampPalette(c("white", "red"))(256), 
        xlab = "Athlete", ylab = "Discipline")

?cm.colors
# df_crf3 <- as.data.frame(mat_crf3)
# 
# ggplot(df_crf3, aes())+ 
#   geom_tile(aes(fill = rescale),colour = "white")

# pretty heatmap
?pheatmap
pheatmap(mat_crf3, color = colorRampPalette(c("white", "red"))(256), kmeans_k =  NA, 
         cluster_rows = FALSE, cluster_cols = FALSE,
         labels_col = c("Athlete 1","Athlete 2","Athlete 3","Athlete 4"), 
         labels_row = c("swim","transition 1","bike","transition 2", "run"), 
         display_numbers = F, fontsize = 20, number_color = "gray", angle_col = "45",
         legend = T, a)

# levelplot
require(lattice)
levelplot(mat_crf3, scale=list(x=list(rot=0)), col.regions = colorRampPalette(c("white", "darkred"))(100),
          xlab = list(label = "Discipline", cex = 1.5), ylab = "", ylab.right = list(label = "Importance Score", cex = 1.5), 
          par.settings = list(layout.widths = list(axis.key.padding = 0,ylab.right = 3)), aspect = 1/3,
          scales=list(x=list(cex=1.2),y=list(cex=1.2)), colorkey = list(labels=list(cex=1)))
?levelplot

# with new matrix - simpler heatmap
mat_crf3_2 <- mat_crf3[c(1,3,5),]
mat_crf3_2 <- mat_crf3_2[,c("Athlete 4","Athlete 3","Athlete 2","Athlete 1")]

levelplot(mat_crf3_2, scale=list(x=list(rot=0)), col.regions = colorRampPalette(c("white", "darkred"))(100),
          xlab = list(label = "Discipline", cex = 1.7), ylab = "", ylab.right = list(label = "Importance Score", cex = 1.7), 
          par.settings = list(layout.widths = list(axis.key.padding = 0,ylab.right = 3)), aspect = 1/1.5,
          scales=list(x=list(cex=1.5),y=list(cex=2)), colorkey = list(labels=list(cex=1.7)))


### end ####

setwd ("\\\\adb.intra.admin.ch/Userhome$/BASPO-01/U80822632/config/Desktop/R")

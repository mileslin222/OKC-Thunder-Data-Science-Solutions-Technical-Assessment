rm(list = ls())
install.packages("dplyr")
library(dplyr)

#build up data frame form csv file
sdata<-read.csv("/Users/miles/Desktop/shots_data.csv", header=TRUE)

#add "dist" column for shooting distance 
sdata$dist <- sqrt(sdata$x^2+sdata$y^2)
#add "zone" column to categorize the shot zones and set the original value be "2 Point". 
sdata$zone <- "2 Point"
#replace "2 Point" with "Corner 3" for "Corner 3" in column "zone" when y value <= 7.8 and x value >22
sdata$zone[sdata$y <=7.8 & sdata$x > 22] <- "Corner 3"
#replace "2 Point" with "NCorner 3" for "Non Corner 3"in column "zone" when y value > 7.8 and "dist"(shooting distance) value >23.75
sdata$zone[sdata$y >7.8 & sdata$dist >23.75] <- "NCorner 3"
#separate the data frame from one to two, based on different team 
sdataA <- sdata[sdata$team == "Team A",]
sdataB <- sdata[sdata$team == "Team B",]


#count the total shot attempts from each zone

#total shot attempts for Two Point for Team A
A_2pt_atp <- sum(sdataA$zone == '2 Point')
#total shot attempts for Corner Three for Team A
A_c3_atp <- sum(sdataA$zone == 'Corner 3')
#total shot attempts for Non Corner Three for Team A
A_nc3_atp <- sum(sdataA$zone == 'NCorner 3')
#total shot attempts for Team A
A_tot_atp <-(A_2pt_atp + A_c3_atp + A_nc3_atp)

#Calculate the percentage of Team A shots attempted within a zone

#Two Point shot distribution for Team A, Rounded to 3 decimal places 
A_2pt_distr <- round(A_2pt_atp / A_tot_atp,3)
#Non Corner 3 shot distribution for Team A, Rounded to 3 decimal places 
A_c3_distr <- round(A_c3_atp / A_tot_atp,3)
#Corner 3 shot distribution for Team A, Rounded to 3 decimal places
A_nc3_distr <- round(A_nc3_atp / A_tot_atp,3)

#total shot attempts for Two Point for Team B
B_2pt_atp <- sum(sdataB$zone == '2 Point')
#total shot attempts for Corner Three for Team B
B_c3_atp <- sum(sdataB$zone == 'Corner 3')
#total shot attempts for Non Corner Three for Team B
B_nc3_atp <- sum(sdataB$zone == 'NCorner 3')
#total shot attempts for Team B
B_tot_atp <-(B_2pt_atp + B_c3_atp + B_nc3_atp)

#Calculate the percentage of Team B shots attempted within a zone

#Two Point shot distribution for Team B, Rounded to 3 decimal places
B_2pt_distr <- round(B_2pt_atp / B_tot_atp,3)
#Corner 3 shot distribution for Team B, Rounded to 3 decimal places
B_c3_distr <- round(B_c3_atp / B_tot_atp,3)
#Non Corner 3 shot distribution for Team B, Rounded to 3 decimal places
B_nc3_distr <- round(B_nc3_atp / B_tot_atp,3)

#eFG calculation

#Two Point made by Team A (when zone is '2 Point' and fgmade is '1'.)
A_2pt_md <- sum(sdataA$zone == '2 Point' & sdataA$fgmade ==1)
#Corner Three made by Team A (when zone is 'Corner 3' and fgmade is '1'.)
A_c3_md <- sum(sdataA$zone == 'Corner 3' & sdataA$fgmade ==1)
#Non Three made by Team A (when zone is 'NCorner 3' and fgmade is '1'.)
A_nc3_md <- sum(sdataA$zone == 'NCorner 3' & sdataA$fgmade ==1)

#Two Point eFG for Team A, Rounded to 3 decimal places
A_eFG_2pt <- round(A_2pt_md / A_2pt_atp,3)
#Non Corner Three eFG for Team A, Rounded to 3 decimal places
A_eFG_nc3 <- round((A_nc3_md + 0.5*A_nc3_md) / A_nc3_atp,3)
#Corner Three eFG for Team A, Rounded to 3 decimal places
A_eFG_c3 <- round((A_c3_md + 0.5*A_c3_md) / A_c3_atp,3)

#Two Point made by Team B (when zone is '2 Point' and fgmade is '1'.)
B_2pt_md <- sum(sdataB$zone == '2 Point' & sdataB$fgmade ==1)
#Corner Three made by Team B (when zone is 'Corner 3' and fgmade is '1'.)
B_c3_md <- sum(sdataB$zone == 'Corner 3' & sdataB$fgmade ==1)
#Non Corner Three made by Team B (when zone is 'NCorner 3' and fgmade is '1'.)
B_nc3_md <- sum(sdataB$zone == 'NCorner 3' & sdataB$fgmade ==1)
#Two Point eFG for Team B, Rounded to 3 decimal places
B_eFG_2pt <- round(B_2pt_md / B_2pt_atp,3)
#Non Corner Three eFG for Team B, Rounded to 3 decimal places
B_eFG_nc3 <- round((B_nc3_md + 0.5*B_nc3_md) / B_nc3_atp,3)
#Corner Three eFG for Team B, Rounded to 3 decimal places
B_eFG_c3 <- round((B_c3_md + 0.5*B_c3_md) / B_c3_atp,3)

#Code was produced in R
library(tidyverse)

#Read in as a .csv file for easier data manipulation.
#Choose file fastballcommand.csv
fastcmd<-read.csv(file.choose(), header=T)
attach(fastcmd)

#Finding the index of points that don't have co-ordinates (ie. Can't plot them)
droppt=which(fastcmd$PlateLocHeight=="NULL")

#Remove "NULL" points as they don't have a coordinate to plot, clean data
fastcmd<-fastcmd[-c(droppt),]

#Plot of all points overlapping eachother, not informative.
plot(PlateLocSide,PlateLocHeight,col="red",xlab="Pitch X-Coordinate (in ft)",
     ylab="Pitch Height (in ft)",main="Every Pitch Plotted In Relation to Strikezone")

#Split the Pitchers by their ID, makes no sense to plot all the pitches as if
#they were one.
pitcher_split<-split(fastcmd,PitcherID)

#A "Home" Plate is 17 inches across the front, which corresponds to the strikezone
#If 0 is the reference point, divide 17 inches by 2 and figure out how many feet that is
#ie. If middle of the plate is reference point, 0, from middle to left side of plate
#is -0.708333 feet and 0.708333 feet on the right side.
#Average Height of strike zone for MLB Players was taken from an article from
#Baseball Prospectus, where we use the Universal Strikezone used by Pitchgrader
#Dimensions of the strike zone and diameter of the baseball.
#The diameter of a Major League Baseball is 76mm or in feet, that would be
#0.249344 feet.
bbdiameter=0.249344
right_of_sz=0.708333
left_of_sz=(-0.708333)
top_of_sz=3.5508333
bottom_of_sz=1.6466667

#This is all the pitches over lapped. Still messy, but more to plot for reference.
for (p in 1:5){
  points(pitcher_split[[p]]$PlateLocSide,pitcher_split[[p]]$PlateLocHeight,col=p+1
         ,pch=16)

  rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)
}

#Legend
legend("bottomright",c("Pitcher ID: 857","Pitcher ID: 1594","Pitcher ID: 2696",
                  "Pitcher ID: 2779","Pitcher ID: 114013","Strike Zone"), 
       cex=1.1, pt.cex=1.2,pch=16,y.intersp = 0.26,col=c(2,3,4,5,6,1))

#Separating the Pitch Locations by Pitcher, overlapping theoretical strikezone
par(mfrow=c(2,3))
for(q in 1:5) {
  plot(pitcher_split[[q]]$PlateLocSide,pitcher_split[[q]]$PlateLocHeight,col=q+1,
       pch=16, xlab="Pitch X-Coordinate (in ft)", ylab="Pitch Height (in ft)")
  title(paste("Fastball Location for Pitcher ID:",pitcher_split[[q]]$PitcherID[1]))
  rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)
}

#Legend
plot(NA,xlim=c(0,1),ylim=c(0,1), axes=FALSE, xlab="",ylab="")
legend("bottomleft",c("Pitcher ID: 857","Pitcher ID: 1594","Pitcher ID: 2696",
                  "Pitcher ID: 2779","Pitcher ID: 114013", "Strike Zone"), 
       cex=1.6, pt.cex=1.6,pch=16,bty="n", y.intersp = 0.3 ,col=c(2,3,4,5,6,1))

#Use rejection regions (rectangular) to see if the pitch is deemed a strike or not.
#We seperate by pitcher and take into consideration the diameter of a baseball
#Any part of the ball that catches the "black" of the plate is considered a strike.

#Number of Pitches each pitcher threw.
num_pitch=c(4528,1520,2246,2233,1947)
ITER=NA

#Variables to Keep Track of the Type of Pitch (Ball or Strike), and the indices
#of what pitch was a ball and strike for plotting purposes later.
pitcher_cnt=list()
ball_index1=ball_index2=ball_index3=ball_index4=ball_index5=numeric()
strike_index1=strike_index2=strike_index3=strike_index4=strike_index5=numeric()
cnt=array()
ball=numeric()
strike=numeric()

#Outer loop essentially just picks the pitcher 1 through 5.
for(a in 1:length(num_pitch)){
  #Specifying the number of pitches thrown by each pitcher for proper iterations
  #in the inner for loop.
  if(a==1){
    ITER=num_pitch[1]
  }
  if(a==2){
    ITER=num_pitch[2]
  }
  if(a==3){
    ITER=num_pitch[3]
  }
  if(a==4){
    ITER=num_pitch[4]
  }
  if(a==5){
    ITER=num_pitch[5]
  }
  #Based on the pitcher selected, evaluates each pitch thrown to see strike or ball.
  for(b in 1:ITER){
    #Booleans to see if the ball is within the strikezone, based on x and y co-ordinates.
    #If either gets set to FALSE, the pitch is a ball and gets recorded as such.
    XCO=TRUE
    YCO=TRUE
    #Following two "if" statements check if the ball is outside the theoretical strikezone.
    if(as.numeric(pitcher_split[[a]][b,17])>(right_of_sz+bbdiameter)||(as.numeric(pitcher_split[[a]][b,17]))<(left_of_sz-bbdiameter)){
      cnt[b]="BALL"
      XCO=FALSE
      ball[b]=b
    }
   
    if ((as.numeric(pitcher_split[[a]][b,16])<(bottom_of_sz-bbdiameter))||(as.numeric(pitcher_split[[a]][b,16])>(top_of_sz+bbdiameter))){
      cnt[b]="BALL"
      YCO=FALSE
      ball[b]=b
    }
    #If the pitch is "within the zone" theoretically, called a strike.
    if (XCO==TRUE && YCO==TRUE){
      cnt[b]="STRIKE"
      strike[b]=b
    }
  }  
  #Counter to tally the balls and strikes thrown by a pitcher
  pitcher_cnt[[a]]=cnt
  
  #Based on the Pitcher, record which pitch is considered a ball and strike.
  if(a==1){
    ball_index1=ball
    strike_index1=strike
  }
  if(a==2){
    ball_index2=ball
    strike_index2=strike
  }
  if(a==3){
    ball_index3=ball
    strike_index3=strike
  }
  if(a==4){
    ball_index4=ball
    strike_index4=strike
  }
  if(a==5){
    ball_index5=ball
    strike_index5=strike
  }
  #Reset counters for each pitcher
  cnt=NA
  ball=NA
  strike=NA
}

#We want to see the proportion of strikes a pitcher throws 
#ie. Command of the Strikezone
#This loops takes the number of strikes thrown by the pitcher and divides by
#the total number of pitches thrown.
pitch_result=matrix(NA,nrow=5,ncol=1)
for (c in 1:5){
  pitch_result[c,1]=as.matrix(table(pitcher_cnt[[c]]))[2,1]/num_pitch[c]
}

#Based on these results, Pitcher 5 or Pitcher ID: 114013 had the best command.
rownames(pitch_result)=c("Pitcher ID: 857","Pitcher ID: 1594","Pitcher ID: 2696",
                         "Pitcher ID: 2779","Pitcher ID: 114013")
colnames(pitch_result)=c("% Strikes")
pitch_result

#Getting rid of NA's for Pitcher 1 in Ball and Strike indices, needed for proper plotting
Ball_filter<-which(is.na(ball_index1))
Strike_filter<-which(is.na(strike_index1))
ball_index1=ball_index1[-c(Ball_filter)]
strike_index1=strike_index1[-c(Strike_filter)]

#Pitcher 2
Ball_filter<-which(is.na(ball_index2))
Strike_filter<-which(is.na(strike_index2))
ball_index2=ball_index2[-c(Ball_filter)]
strike_index2=strike_index2[-c(Strike_filter)]

#Pitcher 3
Ball_filter<-which(is.na(ball_index3))
Strike_filter<-which(is.na(strike_index3))
ball_index3=ball_index3[-c(Ball_filter)]
strike_index3=strike_index3[-c(Strike_filter)]

#Pitcher 4
Ball_filter<-which(is.na(ball_index4))
Strike_filter<-which(is.na(strike_index4))
ball_index4=ball_index4[-c(Ball_filter)]
strike_index4=strike_index4[-c(Strike_filter)]

#Pitcher 5
Ball_filter<-which(is.na(ball_index5))
Strike_filter<-which(is.na(strike_index5))
ball_index5=ball_index5[-c(Ball_filter)]
strike_index5=strike_index5[-c(Strike_filter)]

#Plot the strikes and balls
par(mfrow=c(2,3))

#Pitcher 1
plot(pitcher_split[[1]][ball_index1,17],pitcher_split[[1]][ball_index1,16],col="blue",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Balls and Strikes of Pitcher ID: 857")
points(pitcher_split[[1]][strike_index1,17],pitcher_split[[1]][strike_index1,16],col="red")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 2
plot(pitcher_split[[2]][ball_index2,17],pitcher_split[[2]][ball_index2,16],col="blue",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Balls and Strikes of Pitcher ID: 1594")
points(pitcher_split[[2]][strike_index2,17],pitcher_split[[2]][strike_index2,16],col="red")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 3
plot(pitcher_split[[3]][ball_index3,17],pitcher_split[[3]][ball_index3,16],col="blue",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Balls and Strikes of Pitcher ID: 2696")
points(pitcher_split[[3]][strike_index3,17],pitcher_split[[3]][strike_index3,16],col="red")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 4
plot(pitcher_split[[4]][ball_index4,17],pitcher_split[[4]][ball_index4,16],col="blue",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Balls and Strikes of Pitcher ID: 2779")
points(pitcher_split[[4]][strike_index4,17],pitcher_split[[4]][strike_index4,16],col="red")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 5
plot(pitcher_split[[5]][ball_index5,17],pitcher_split[[5]][ball_index5,16],col="blue",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Balls and Strikes of Pitcher ID: 114013")
points(pitcher_split[[5]][strike_index5,17],pitcher_split[[5]][strike_index5,16],col="red")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Legend
plot(NA,xlim=c(0,1),ylim=c(0,1), axes=FALSE, xlab="",ylab="")
legend("center",c("Ball","Strike","Strike Zone"), 
       cex=1.6, pt.cex=1.4,pch=16, col=c("blue","red","black"))

#BUT, Fastball Command isn't just about getting in the strike-zone, more so, get
#pitch to "hit the corners" and "paint black"

#Readjust our loops to see how close pitches are to border of strikezone.
ITER=NA

#Similar to above, but instead of looking at balls and strikes, we are looking at
#pitches that "paint" the edges of the strikezone.
pitcher_cnt=list()
paint_index1=paint_index2=paint_index3=paint_index4=paint_index5=numeric()
nopaint_index1=nopaint_index2=nopaint_index3=nopaint_index4=nopaint_index5=numeric()
cnt=array()
paint=numeric()
nopaint=numeric()

#Dimensions of Strikezone and ball diameter do not change from previous loops.
#Outer loop essentially just picks the pitcher 1 through 5.
for(a in 1:length(num_pitch)){
  #Specifying the number of pitches thrown by each pitcher for proper iterations
  #In the inner for loop.
  if(a==1){
    ITER=num_pitch[1]
  }
  if(a==2){
    ITER=num_pitch[2]
  }
  if(a==3){
    ITER=num_pitch[3]
  }
  if(a==4){
    ITER=num_pitch[4]
  }
  if(a==5){
    ITER=num_pitch[5]
  }

  for(b in 1:ITER){
    #Booleans to see if the ball is "painting" the edges based on x and y co-ordinates.
    #If either stays FALSE, the pitch is either a ball or "catching" too much of the plate.
    XCO=FALSE
    YCO=FALSE
    
    #Following four "if" statements check if the ball is within the dimensions
    #of catching the edges of the strikezone or not.
    if((right_of_sz+bbdiameter) > (as.numeric(pitcher_split[[a]][b,17])) && (as.numeric(pitcher_split[[a]][b,17]) > (left_of_sz-bbdiameter))){
      cnt[b]="PAINT"
      XCO=TRUE
    }
    if ((top_of_sz+bbdiameter) > (as.numeric(pitcher_split[[a]][b,16])) && (as.numeric(pitcher_split[[a]][b,16]) > (bottom_of_sz-bbdiameter))){
        cnt[b]="PAINT"
        YCO=TRUE
      }
    
    if((left_of_sz+bbdiameter) < (as.numeric(pitcher_split[[a]][b,17])) && (as.numeric(pitcher_split[[a]][b,17]) < (right_of_sz-bbdiameter))){
      if ((bottom_of_sz+bbdiameter) < (as.numeric(pitcher_split[[a]][b,16])) && (as.numeric(pitcher_split[[a]][b,16]) < (top_of_sz-bbdiameter))){
      cnt[b]="NO PAINT"
      YCO=FALSE
      XCO=FALSE
      }
    }
    #Classifying if the pitch is "PAINT" meaning it caught the edges or "NO PAINT"
    #Meaning it did not.
    if (XCO==FALSE || YCO==FALSE){
      cnt[b]="NO PAINT"
    }
    
    if(cnt[b]=="PAINT"){
      paint[b]=b
    }
    if(cnt[b]=="NO PAINT"){
      nopaint[b]=b
    }
  }
  #Counter to tally the "PAINT" and "NO PAINT" pitches similar to previous loops.
  pitcher_cnt[[a]]=cnt
  
  #Based on the Pitcher, records which pitch is considered PAINT or not.
  if(a==1){
    paint_index1=paint
    nopaint_index1=nopaint
  }
  if(a==2){
    paint_index2=paint
    nopaint_index2=nopaint
  }
  if(a==3){
    paint_index3=paint
    nopaint_index3=nopaint
  }
  if(a==4){
    paint_index4=paint
    nopaint_index4=nopaint
  }
  if(a==5){
    paint_index5=paint
    nopaint_index5=nopaint
  }
  #Reset counters for each pitcher
  cnt=NA
  paint=NA
  nopaint=NA
}


#We want to see the proportion of strikes that are around the strikezone
#ie. True Command of the Fastball.
#Take the number of pitches labelled "PAINT" and divide by total # of pitches.
paint_result=matrix(NA,nrow=5,ncol=1)
for (c in 1:5){
  paint_result[c,1]=as.matrix(table(pitcher_cnt[[c]]))[2,1]/num_pitch[c]
}

#Based on this Pitcher 1 or Pitcher ID: 857 had the best command.
rownames(paint_result)=c("Pitcher ID: 857","Pitcher ID: 1594","Pitcher ID: 2696",
                         "Pitcher ID: 2779","Pitcher ID: 114013")
colnames(paint_result)=c("% on the ``Paint``")
paint_result

#Getting rid of NA's for Pitcher 1 paint or no paint, needed for proper plotting
paint_filter<-which(is.na(paint_index1))
nopaint_filter<-which(is.na(nopaint_index1))
paint_index1=paint_index1[-c(paint_filter)]
nopaint_index1=nopaint_index1[-c(nopaint_filter)]

#Pitcher 2
paint_filter<-which(is.na(paint_index2))
nopaint_filter<-which(is.na(nopaint_index2))
paint_index2=paint_index2[-c(paint_filter)]
nopaint_index2=nopaint_index2[-c(nopaint_filter)]

#Pitcher 3
paint_filter<-which(is.na(paint_index3))
nopaint_filter<-which(is.na(nopaint_index3))
paint_index3=paint_index3[-c(paint_filter)]
nopaint_index3=nopaint_index3[-c(nopaint_filter)]

#Pitcher 4
paint_filter<-which(is.na(paint_index4))
nopaint_filter<-which(is.na(nopaint_index4))
paint_index4=paint_index4[-c(paint_filter)]
nopaint_index4=nopaint_index4[-c(nopaint_filter)]

#Pitcher 5
paint_filter<-which(is.na(paint_index5))
nopaint_filter<-which(is.na(nopaint_index5))
paint_index5=paint_index5[-c(paint_filter)]
nopaint_index5=nopaint_index5[-c(nopaint_filter)]

#Plot the PAINT and NO PAINT pitches
par(mfrow=c(2,3))

#Pitcher 1
plot(pitcher_split[[1]][nopaint_index1,17],pitcher_split[[1]][nopaint_index1,16],col="red",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Pitches on Edges of Zone - Pitcher ID: 857",cex=0.9)
points(pitcher_split[[1]][paint_index1,17],pitcher_split[[1]][paint_index1,16],col="green")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 2
plot(pitcher_split[[2]][nopaint_index2,17],pitcher_split[[2]][nopaint_index2,16],col="red",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Pitches on Edges of Zone - Pitcher ID: 1594")
points(pitcher_split[[2]][paint_index2,17],pitcher_split[[2]][paint_index2,16],col="green")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 3
plot(pitcher_split[[3]][nopaint_index3,17],pitcher_split[[3]][nopaint_index3,16],col="red",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Pitches on Edges of Zone - Pitcher ID: 2696")
points(pitcher_split[[3]][paint_index3,17],pitcher_split[[3]][paint_index3,16],col="green")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 4
plot(pitcher_split[[4]][nopaint_index4,17],pitcher_split[[4]][nopaint_index4,16],col="red",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Pitches on Edges of Zone - Pitcher ID: 2779")
points(pitcher_split[[4]][paint_index4,17],pitcher_split[[4]][paint_index4,16],col="green")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Pitcher 5
plot(pitcher_split[[5]][nopaint_index5,17],pitcher_split[[5]][nopaint_index5,16],col="red",
     xlab="Pitch X-coordinate (in ft)",ylab="Pitch Height (in ft)")
title("Pitches on Edges of Zone - Pitcher ID: 114013")
points(pitcher_split[[5]][paint_index5,17],pitcher_split[[5]][paint_index5,16],col="green")
rect(left_of_sz,bottom_of_sz,right_of_sz,top_of_sz)

#Legend
plot(NA,xlim=c(0,1),ylim=c(0,1), axes=FALSE, xlab="",ylab="")
legend("center",c("Paint","Not Paint","Strike Zone"), 
       cex=1.5, pt.cex=1.4,pch=16, col=c("green","red","black"))

#Proportion of Strikes that were around the zone, "painting" the edges and not 
#just thrown down the middle of the plate.
final_result=paint_result/pitch_result
colnames(final_result)=c("% of Strikes around Strike Zone")
final_result
#Based on this metric, Pitcher 1, Pitcher ID: 857, has the best fastball command.

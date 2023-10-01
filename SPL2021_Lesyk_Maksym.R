################################################################################ 
################## Statistical Programming Languages 2020/21 ###################
##################               Take-home Exam              ###################
##################                                           ###################
##################       Eva-Maria Maier, Elena Ivanova      ###################
################################################################################

#----------------------------------------------------------------
# Surname: Lesyk
# Name: Maksym
# Student ID (Matrikelnummer): 614228
#----------------------------------------------------------------

### Exercise 1 --------------------------------------------------

## SessionInfo() includes basic information about the R version used
## and under which version it is running. 
## "R version", "Platform" and "Running under" are self-explainatory as they
## define the basic features of the system. 
## "Matrix products" define that default BLAS and LAPACK are used for computations.
## These contain algorithms to which R adheres while performing its calculations.
## There are also paths to appropriate BLAS and LAPACK files.
## "Locale" specifies the languages and character encoding systems used.
## "Attached base packages" points out downloaded base packages of R.
## "Loaded via a namespace (and not attached)" means packages that are additionally
## used by the base packages.

### Exercise 2 --------------------------------------------------
#a
## set a working directory
setwd("~/Desktop/SPL homework")
## read the file
crDataset= read.csv("corona.csv")
## delete unnecessary columns
crDataset$day=NULL
crDataset$month=NULL
crDataset$year=NULL
crDataset$geoId=NULL
crDataset$countryterritoryCode=NULL
## check the names of the dataset
names(crDataset)
## change the names of the columns
names(crDataset)[1]="date"
names(crDataset)[6]="continent"
names(crDataset)[7]="indicator14"
## change the format to POSIX and check the class
crDataset$date=as.POSIXct(crDataset$date, format="%d/%m/%Y")
class(crDataset$date)

#b
table(is.na(crDataset))
## a matrix is created thas shows (under the heading TRUE that there are
## 2936 NAs in total for the whole dataset)

#c
## sum the cases
sum(crDataset$cases >200)
length(crDataset$cases)
## find the percantage
sum(crDataset$cases >200)/length(crDataset$cases)

#d
## choose appropriate subsets
Spain = subset(crDataset, crDataset$countriesAndTerritories == "Spain" & crDataset$date <= as.POSIXct("31-3-2020",format= "%d-%m-%Y") & crDataset$date >= as.POSIXct("1-3-2020",format= "%d-%m-%Y"))
Italy = subset(crDataset, crDataset$countriesAndTerritories == "Italy" & crDataset$date <= as.POSIXct("31-3-2020",format= "%d-%m-%Y") & crDataset$date >= as.POSIXct("1-3-2020",format= "%d-%m-%Y"))
## perform t-test to determine significance levels based on the p-value
TT = t.test(Spain$indicator14, Italy$indicator14, paired = TRUE)
TT$p.value

## the significance level is above the threshold. We may conclude that the two samoles are similar.

#e
## change the formats to POSIX
October = subset(crDataset, crDataset$date <= as.POSIXct("31-10-2020",format= "%d-%m-%Y") & crDataset$date >= as.POSIXct("1-10-2020",format= "%d-%m-%Y"))
x = aggregate(deaths ~ continent, data=October, sum)
y = aggregate(deaths ~ continent, data=crDataset, sum)
x[order( x[,2]), ]
y[order( y[,2]), ]
#Europe and Asia are mixed in October. Otherwise the order is the same in both cases.

#f
## create an appropriate moving average to substitute the indicator
Germany = subset(crDataset, crDataset$countriesAndTerritories == "Germany")
Germany = Germany[order(Germany$date), ]
movAvr <- function(x,n=7){stats::filter(x,rep(1/n,n), sides=1)}
Germany$indicator7 = movAvr(Germany$deaths)

#g

### Exercise 3 --------------------------------------------------
## read the dataset
dataset3=state.x77
dataset3=as.data.frame(dataset3)
## delete columns
dataset3$Income=NULL
dataset3$`Life Exp`=NULL
dataset3$`HS Grad` =NULL
dataset3$Frost=NULL
dataset3$Area=NULL
## create graphs and save them
pdf("R_plot.pdf") 
dataset3 = dataset3[order(dataset3$Population,decreasing = TRUE), ]
layout(matrix(c(1,4,3,2), 2, 2, byrow = TRUE),widths=c(1.9,1),heights=c(1,1.7))
barplot(dataset3$Illiteracy[1:5], main = "The five biggest states", names.arg =c("Calif","NY","Texas","Penns","Illin"), ylab = "Illiteracy", ylim = c(0,3), width=c(1.5,1.5,1.5,1.5,1.5))
boxplot(dataset3$Illiteracy, col = "red", ylab = "Illiteracy")
plot(Murder~Illiteracy, data=dataset3, pch = 4, col ="green")
text(Murder~Illiteracy, labels=rownames(dataset3),data=dataset3, cex=0.7, pos=4, col="red", font=2)
dependence= lm(formula = Murder~Illiteracy, data=dataset3)
abline(dependence)
dev.off() 
### Exercise 4 --------------------------------------------------
#a
# Set the working directory to an appropriate working directory
setwd("~/Desktop/SPL homework")
# read the txt. files and convert them to dataframes
guardian=read.delim("guardian.txt", header = TRUE, sep = "\t", dec = ".")
bbc=read.delim("bbc.txt", header = TRUE, sep = ";", dec = ".")
guardian=as.data.frame(guardian)
bbc=as.data.frame(bbc)
# delete the leading and trailing whitespaces 
guardian=data.frame(lapply(guardian,trimws))
bbc=data.frame(lapply(bbc,trimws))
# check that the strings are of the class character
summary(guardian)
summary(bbc)
#b
# delete the columns that are missing in either the bbc or guardian files
guardian$feed_category=NULL
bbc$feed_last_build_date=NULL
bbc$feed_generator=NULL
bbc$feed_ttl=NULL
#connect the two feeds together 
rss_feed=rbind(bbc,guardian)
rss_feed$item_pub_date=as.POSIXct(rss_feed$item_pub_date)
# check that he two dataframes are appropariately connected
summary(rss_feed)
# sort 
rss_feed=rss_feed[order(rss_feed$item_pub_date),]
#c
install.packages("stringr")
library("stringr") 
rss_feed$feed_title=trimws(str_replace(rss_feed$feed_title,"\\s-.+",""))
#d
## count the number of occurances of the words
count_occurances_in_feed=str_detect(rss_feed$item_title, regex("corona|pandemic|covid|lockdown|wave|vaccine", ignore_case = TRUE))
x=as.numeric(sum(count_occurances_in_feed =="TRUE"))
y=as.numeric(sum(count_occurances_in_feed =="FALSE"))
x/(x+y)
#e
## count the number of occurances of numbers and articles
count_occurances_in_feed=str_detect(rss_feed$item_description, regex("^a\\s|^the\\s", ignore_case = TRUE))
x=as.numeric(sum(count_occurances_in_feed =="TRUE")) ## 19
count_occurances_in_feed_num=str_detect(rss_feed$item_description, regex("[1-9]"))
y=as.numeric(sum(count_occurances_in_feed_num =="TRUE")) ##87
### Exercise 5 --------------------------------------------------

### Exercise 6 --------------------------------------------------
#Functions

AssessResults = function(Mat) {
  MatBU = Mat
  mp1w = "Player 1 wins!"
  mp2w = "Player 2 wins!"
  mdraw = "Its a draw"
  NC = ncol(Mat)
  NR = nrow(Mat)
  xp = NR
  yp = NC - 3
  Flip = 0
  Refl = 0
  iter = 0
  #Assess diagonal victory
  repeat {
    repeat {
      #Check diagonal in right/up direction
      xt = xp
      yt = yp
      P1Sc = 0
      P2Sc = 0
      repeat {
        #Count scores / break if score > 3
        if (Mat[xt, yt] == 1) {
          P1Sc = P1Sc + 1
          P2Sc = 0
        } else if (Mat[xt, yt] == 2) {
          P2Sc = P2Sc + 1
          P1Sc = 0
        } else {
          P1Sc = 0
          P2Sc = 0
        }
        #print(P1Sc)
        #print(P2Sc)
        if (P1Sc == 4) {
          return (mp1w)
        } else if (P2Sc == 4) {
          return (mp2w)
        }
        #Check if position is on lower boundary for x dim or upper boundary for y dim / else shift to next value diagonally
        if (yt == NC || xt == 1) {
          break
        }
        yt = yt + 1
        xt = xt - 1 
      }
      #Shift start position (last row or first column) / Introduced iter to prevent endless loop when provided x/y dimensions < 4
      if (xp == 4 || iter == 300) {
        break
      } else if (yp == 1) {
        Flip = 1
      } 
      if (Flip == 0) {
        yp = yp - 1 
      } else {
        xp = xp - 1
      }
      iter = iter +1
    }
    if (Refl == 0) {
      Mat = Mat[,c(NC:1)]
      Refl = 1
    } else {
      break
    }
  }
  #Assess vertical/horizontal victory
  Rotated = 0
  repeat {
    for (y in seq(from=1, to=NC, by=1)) {
      P1Sc = 0
      P2Sc = 0
      for (x in seq(from=NR, to=1, by=-1)) {
        if (MatBU[x, y] == 1) {
          P1Sc = P1Sc + 1
          P2Sc = 0
        } else if (MatBU[x, y] == 2) {
          P2Sc = P2Sc + 1
          P1Sc = 0
        } else {
          P1Sc = 0
          P2Sc = 0
        }
        if (P1Sc == 4) {
          return (mp1w)
        } else if (P2Sc == 4) {
          return (mp2w)
        }
      }
    }
    if (Rotated == 0) {
      MatBU = rotate(MatBU)
      Rotated = 1
      G = NC
      NC = NR
      NR = G
    } else {
      break
    }
  }
  #check if draw
  if (is.element(0, Mat) == FALSE) {
    return (mdraw)
  }
  return (FALSE)
}

rotate <- function(x) {
  x = t(apply(x, 2, rev))
  return (x)     
}

MakeMove = function(x, Mat, IC=0) {
  if (IC==0) {
    IN = as.integer(readline(prompt="Column for your move")) 
  } else {
    IN = IC   
  }    
  if (Mat[1, IN] != 0) {
    print ("Column not valid. Again:")
    return (MakeMove(x, Mat, IC))
  }
  for (i in seq(from=nrow(Mat), to=1, by=-1)) {
    if (Mat[i, IN] == 0) {
      break
    }
  }
  Mat[i, IN] = x
  return (Mat)
}

#End functions

#Main game start
nc=as.integer(readline(prompt="Specify x dimension"))
nr=as.integer(readline(prompt="Specify y dimension"))
Opt=as.integer(readline(prompt="How many players? (1/2)"))
print("Player 1 starts! In each move you have to choose one column.")

GE = FALSE
iter = 0


#--------------------------------------PLOT
plot(0:0, type="n", font =1, ylim=c(0,nr), xlim=c(0,nc), xlab="", ylab="", xaxs="i",yaxs="i",xaxt="none", yaxt="none")
title("Connect four", line = 2.5)
#xaxt="none", yaxt="none"
vecc=c(1:nc)
vecr=c(1:nr)
pos_vecc=c(1:nc)-0.5
pos_vecr=c(1:nr)-0.5
axis(3,labels=vecc, at=pos_vecc, tick=FALSE)
axis(2,labels=vecr,at=pos_vecr, tick=FALSE)
grid(nx=nc,ny=nr)
#-----------------PE

MatM = matrix(rep(0, nc*nr), nc, nr)

repeat {
  
  #P1 Move
  MatM = MakeMove(1, MatM)
  print(MatM)
  print("Player 2:")
  
  MatP = rotate(MatM)
  for (x in seq(from=1, to=nrow(MatP), by=1)) {
    for (y in seq(from=1, to=ncol(MatP), by=1)) {
      if (MatP[x,y] == 0) {
      } else if (MatP[x,y] == 1) {
        xx=0.5+1*(x-1)
        yy=0.5+1*(y-1)
        points(x = xx, y = yy, col = "black", pch = 16, cex = 7)
      } else {
        xx=0.5+1*(x-1)
        yy=0.5+1*(y-1)
        points(x = xx, y = yy, col = "pink", pch = 16, cex = 7)
      }
    }
  }
  #Assess results
  res = AssessResults(MatM)
  if (res != FALSE) {
    print(res)
    break
  }
  
  #End Move
  
  #P2/Computer Move
  if (Opt == 1) {
    #Computer
    Counter1=0
    Counter2=0
    move_made=0
    if (move_made==0) {
      rand_int=sample.int(nc,1)
      MatM = MakeMove(2, MatM, rand_int)
    } else {
      move_made=0
      next
    }
    for (y in seq(from = 1, to = ncol(MatM), by = 1)) {
      for (x in seq(from=nrow(MatM), to = 1, by = -1)){
        if (MatM[x,y] == 1) {
          Counter1 = Counter1+1
          Counter2 = 0
          print(Counter1)
        } else if (MatM[x,y] == 2) {
          Counter2 = Counter2+1
          Counter1 = 0
        }
      }
      if (Counter1 >= 3 || Counter2 >= 3) {
        print(Counter1)
        print(Counter2)
        target_column=y
        MatM = MakeMove(2, MatM, target_column)
        move_made=1
        print(y)
        break  
      }
      Counter1 = 0
      Counter2 = 0
    }
  } else {
    #P2
    MatM = MakeMove(2, MatM)
  }
  print(MatM)
  print("Player 1:")
  
  MatP = rotate(MatM)
  for (x in seq(from=1, to=nrow(MatP), by=1)) {
    for (y in seq(from=1, to=ncol(MatP), by=1)) {
      if (MatP[x,y] == 0) {
      } else if (MatP[x,y] == 1) {
        xx=0.5+1*(x-1)
        yy=0.5+1*(y-1)
        points(x = xx, y = yy, col = "black", pch = 16, cex = 7)
      } else {
        xx=0.5+1*(x-1)
        yy=0.5+1*(y-1)
        points(x = xx, y = yy, col = "pink", pch = 16, cex = 7)
      }
    }
  }
  #Assess results
  res = AssessResults(MatM)
  if (res != FALSE) {
    print(res)
    break
  }
  #if (iter == 300) {
  #    break
  #}
  iter = iter + 1
}

print("Thanks for playing")

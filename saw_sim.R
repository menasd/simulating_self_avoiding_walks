######## Simulating Self Avoiding Walks ######
library(dplyr)
library(ggplot2)
require(scales)

#Get position status:
get.pos.state <- function(lattice,pos,move){
  
  if(move == "left"){
    next.move <- pos + c(0,-1) 
  }else if(move == "right"){
    next.move <- pos + c(0,1)
  }else if(move == "up"){
    next.move <- pos + c(1,0)
  }else if(move == "down"){
    next.move <- pos + c(-1,0)
  }else{
    next.move <- pos
  }
  
  if(next.move[1] < 0 | next.move[1] > (nrow(lattice)-1)){ 
    return(-1)}
  else{ 
    if(next.move[2] < 0 | next.move[2] > (ncol(lattice)-1)){ 
      return(-1)}
    else {
      return(lattice[next.move[1]+1,next.move[2]+1])
    }
  }
}


#Function to find possible moves:
opt.num <- function(lattice, pos){
  
  next.step.opt <- c(NA)
  
  if(get.pos.state(lattice,pos,"left") == 0)   next.step.opt <- c(next.step.opt, "left")
  if(get.pos.state(lattice,pos,"right") == 0)  next.step.opt <- c(next.step.opt, "right")
  if(get.pos.state(lattice,pos,"up") == 0)     next.step.opt <- c(next.step.opt, "up")
  if(get.pos.state(lattice,pos,"down") == 0)   next.step.opt <- c(next.step.opt, "down")
  
  return(next.step.opt[-1])
}


#Make move
make.move <- function(lattice,pos,direction){
  
  if(counter == 0) lattice[pos[1]+1,pos[2]+1] <- counter + 1
  else lattice[pos[1]+1,pos[2]+1] <- counter
  
  new.pos <- pos
  
  if(direction == "left"){
    new.pos <- pos + c(0,-1) 
  }else if(direction == "right"){
    new.pos <- pos + c(0,1)
  }else if(direction == "up"){
    new.pos <- pos + c(1,0)
  }else if(direction == "down"){
    new.pos <- pos + c(-1,0)
  }
  
  if(new.pos[1] <= (nrow(lattice)-1) &
     new.pos[2] <= (ncol(lattice)-1)){
    
    lattice[new.pos[1]+1,new.pos[2]+1] <- 1
    return(list(lattice=lattice,pos=new.pos))
  }else{
    cat("make.move position impossible")
    return(NA)
  }
}

#################################

n        <- 10
reps     <- 10^7

#Saves number of steps, and prod(1/p(ri))
count.prob <- matrix(NA,nrow=reps,ncol=2)
colnames(count.prob) <- c("counter","ri")
#Saves last coordinate of walk (to find (n,n) later)
last.coord <- matrix(NA,nrow=reps,ncol=2)
colnames(last.coord) <- c("vertical","horizontal")
#Saves each step:
steps <- rep(list(NA),reps)

lattice.path <- "C:/Users/menaS/Documents/classes/STATS202C/project1/lattices"
setwd(lattice.path)
lattice88 <- lattice

lattice <- matrix(0,nrow=n,ncol=n)
for(i in 1:reps){
  
lattice      <- matrix(0,nrow=n,ncol=n)
pos          <- c(0,0)
counter      <- 0
lattice[1,1] <- 1
step.prob    <- c(NA)
  
while(length(possibilities <- opt.num(lattice,pos)) > 0){
  #Calculate probability at each step:
  step.prob <- c(step.prob,length(possibilities))
  #Choose direction randomly
  next.direction <- sample(possibilities,1)
  #Change position in that direction
  change.pos  <- make.move(lattice,pos,next.direction)
  lattice     <- change.pos[["lattice"]]
  pos         <- change.pos[["pos"]]
  
  counter <- counter + 1

}

all.steps       <- step.prob[-1]
count.prob[i,1] <- counter
count.prob[i,2] <- 1/prod(all.steps)
last.coord[i,]  <- pos
steps[[i]]      <- all.steps

if(i %% 1000 == 0) cat(i,"\n")
}

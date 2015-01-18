## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeCacheMatrix does the following four things:
## 1) set the value of matrix
## 2) get the value of matrix
## 3) set the value of inverse matrix
## 4) get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv<-NULL
  set<-function(matrix){##set the value of matrix
    x<<-matrix
    Inv<<-NULL
  }
  get<-function() x ##get the value of matrix
  setInv<-function(I) Inv<<-I ## set the value of inverse matrix
  getInv<-function() Inv ## get the value of inverse matrix
  list(set=set,get=get,setInv=setInv,getInv=getInv)##return a list of four functions
}


## Write a short comment describing this function
## Function cacheSolve calculates the inverse matrix created by makeCacheMatrix
## It first checks to see whether the inverse matrix has been calculated.
## If yes, it gets the inverse matrix and skips the calculation.
## Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv<-x$getInv()
  if(!is.null(Inv)){ ## If already calculated
    message("getting cached data")
    return(Inv)
  }
  ##Otherwise calculate the inverse
  data<-x$get()
  Inv<-solve(data,...)
  x$setInv(Inv)
  Inv
}

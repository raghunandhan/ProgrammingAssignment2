## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the matrix
## get the matrix
## set inverse of the matrix
## get the inverse of the matrix
## functions do

makeCacheMatrix <- function(x = matrix()) 
{
  m<-matrix(nrow=nrow(x),ncol=ncol(x)) # matrix of "NA"
  print(m)
  set<-function(y)
  {
    x<<-y
    m<<-matrix(nrow=nrow(x),ncol=ncol(x)) # matrix of "NA"
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Return a matrix that is the inverse of 'x'
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
  m<-x$getinverse()
  if(sum(is.na(m[,])==0)) 
  {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m       
}

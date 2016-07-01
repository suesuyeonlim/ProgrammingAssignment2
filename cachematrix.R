## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(inv) i <<- inv
  getinv<-function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)}


## cacheSolve retrieves an inverse of a matrix when the matrix is already set in the above makeCacheMatrix. If not, it is going to calculate one and store it in the above function.

cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}
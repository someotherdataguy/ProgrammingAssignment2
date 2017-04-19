## A pair of functions that allows matrices to be solved and
# the solve results cached

## creates a matrix with a cachable inverse

makeCacheMatrix <- function(x = matrix()) {
  I<-NULL
  set<-function(y){
    x<<-y
    I<<-NULL
  }
  get<-function() x
  setinv<-function(i) I<<-i
  getinv<-function() I
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## solves or delivers a cached solve result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

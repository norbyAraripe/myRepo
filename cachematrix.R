## Once a matrix is created using makeCacheMatrix and assigned to a variable
## e.g. my_matrix <- makeCacheMatrix(matrix(rnorm(25,mean=2),5,5))
## then all the functions from makeCacheMatrix will be stored/cached in my_matrix object
## using whatever we put as argument on the previous function makeCacheMatrix (i.e. 'x')
## therefore here we can call x$getinverse() instead needing to call my_matrix$getinverse
## same applies to other functions wihtin makeCacheMatrix

## MakeCacheMatrix will build set(y),get(),setinverse(inverse) and getinverse() which will accessed
## by cacheSolve() later on with just a reference to 'x' as in x$getinverse()
## the set functions need a <<- for variable m so it can be accessed in a parent environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##this will calculate the inverse of a squared function. 
##Its argument should be an object that was assigned by makeCacheMatrix
##The object will store the cached information that the functions referincing 'x$..." need

cacheSolve <- function(x, ...) {
  m = x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

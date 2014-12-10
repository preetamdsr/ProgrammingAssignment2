## Following functions caches a matrix and it's inverse for efficient use of memory and time

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing different functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invm) inv <<- invm
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {  
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }  
  data <- x$get()  
  inv <- solve(data)  
  x$setinv(inv)
  inv
}

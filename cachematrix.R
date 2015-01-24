## The two functions below create a special "matrix" 
## that computes and caches the inverse the first time the inverse is calculated. 
## Subsequent calls return the cached result speeding up the calculation
##
## An example on how to use the 2 functions is at the bottom of the page.

## Create a special "matrix",  which is a list with the following functions:
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of the inverse
### 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created 
## by makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##    Example 
x <- matrix(c(1,1,1,1,1,2,1,2,1,1,1,0,1,4,2,3), nrow=4, ncol=4)

### creates a cached version x, xC 
xC <- makeCacheMatrix(x)

### First call computes the inverse
xinv1 <- cacheSolve(xC)

### The second call is retrieved from cache
xinv2  <- cacheSolve(xC)

### verify that
x     %*% xinv1 
xinv2 %*%  x

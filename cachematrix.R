## Put comments here that give an overall description of what your
## functions do

## creates an special matrix which can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## creates an inverse using the solve() function to compute an inverse of the makeCacheMatrix function
## but if inversion has already been done it just returns the cache to boost performance

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## TESTING: use the following code in the console to test

## creates a square matrix:
##testmatrix <- matrix(1:4, 3, 3)

##assiging the makeCacheMatrix function to a testing oject runtest:
##runtest <- makeCacheMatrix(testmatrix)

## test for original and inverse: 
##runtest$get() will return the matrix
##cacheSolve(runtest) will return the inverse
## if the last line is run multiple times then the console says" getting cached data" 

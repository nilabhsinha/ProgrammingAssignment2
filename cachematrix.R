## makeCacheMatrix creates a 'list' of functions set, get, setinverse and getinverse.
## cacheSolve checks the matrix, passed as parameter, if same returns the saved inverse value.
## else recalculates the inverse, stores the inverse matrix in an environment variable and 
## returns the new inverse matrix.

## makeCacheMatrix has 2 set of functionalites.
## 1. Creates a special matrix and stores in a different environment.
## 2. Sets the value of the matrix and its inverse matrix (set & setinverse) and 
##    Gets the value of the matrix and its inverse matrix (get & getinverse)

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve checks if the inverse Matrix data is populated. If yes, it does not 
## calculates the inverse and returns the saved/cached matrix. Else it gets the new 
## (changed) matrix and calculates the inverse. Further, it sets the new inverse matrix
## which is finally returned as well.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  inverse <- solve(data, ...)
  
  x$setinverse(inverse)
  
  i
}

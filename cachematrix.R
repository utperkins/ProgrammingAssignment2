##   These two functions will create an inverse matrix, and store that inverse matrix in cache
##     so it only needs to be re-inversed once in a program, or when/if data changes

##   Function #1: makeCacheMatrix

##   Create function to create a special matrix object where its inverse can be cached in memory
##   because creating an inverse of a large matrix several times in one program can be
##   computationally expensive

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## Function #2: cacheSolve 

## Computes the inverse of the matrix returned by Function #1 above.
##   If the inverse has already been calculated, and the data has not changed,
##   then just use the cached matrix

cacheSolve <- function(x, ...) {
        ## Create and return inverse matrix of 'x'
  m <- x$getmatrix()
  if (!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

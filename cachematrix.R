#______________________________________________________________________________
# Assignment: write a pair of functions that cache the inverse of a matrix.
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
# changed), then the cachesolve should retrieve the inverse from the cache.
#______________________________________________________________________________

#______________________________________________________________________________
# makeCacheMatrix function creates a list that stores functions 
# to get and set matrix and its inverse.
# It also uses matrix inverseMatrix to store the inverse matrix.
#______________________________________________________________________________

makeCacheMatrix <- function(x = matrix()) {
  
  #matrix initialization
  inverseMatrix <- NULL 
  
  #sets matrix
  set <- function(y){
    # <<- operator assigns value to an object that is in an 
    # environment different than current
    x <<- y
    inverseMatrix <<- NULL
  }
  
  #gets matrix
  get <- function() x
  
  #set & get inverse
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  
  #make list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

#______________________________________________________________________________
# cacheSolve checks to see if inverse of given matrix exists, and if it does, 
# gets it from the cache.if it doesn't exist, it calculates it.
#______________________________________________________________________________

cacheSolve <- function(x, ...) {
  
  # if inverse exists
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    print(inverseMatrix) 
    return(inverseMatrix)
  }
  
  # if inverse needs to be calculated
  message("calculating inverse matrix")
  data <- x$get()
  #using the solve function to calculate the inverse
  inverseMatrix <- solve(data,...)
  x$setInverse(inverseMatrix)
  print(inverseMatrix)
  return(inverseMatrix)
}
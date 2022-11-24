akeCacheMatrix <- function(x = matrix()) { ##define the argument with default mode of the matrix
  
  inv <- NULL## initialize inv is NULL; will hold value of matrix inverse
  set <- function(y){
    x <<- y    ##value of matrix is parent environment
    inv <<- NULL
  }## return value of the matrix argument
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv ## get the value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##this function computes the inverse of thre special matrix returned by makecachematrix above
## if the inverse has already been calculated

cacheSolve <- function(x, ...) {  ## return the value that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

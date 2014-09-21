
## makeCacheMatrix function caches the matrix passed as argument and also its inverse.
## This returns a list of functions to access and modify the cached matrix and inverse of cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Initalisation of local variable inverseMatrix(stores the inverse of Matrix)
  inverseMatrix <- NULL;
  
  ## function to set matrix
  set <- function(y){
    x <<- y;
    inverseMatrix <<- NULL;
  }
  
  ## function to get matrix
  get <- function(){
   return(x); 
  }
  
  ## function to set Inverse of Matrix
  setMatrixInverse <- function(iMatrix) {
    inverseMatrix <<- iMatrix;
  }
  
  ## function to get Inverse of Matrix
  getMatrixInverse <- function(){
    return(inverseMatrix);
  }
  ## special Cache Matrix
  list(set=set,
       get=get,
       getMatrixInverse=getMatrixInverse,
       setMatrixInverse=setMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  matrixInverse <- x$getMatrixInverse();
  
  ## If inverse matrix present in Cache return cached value
  if(!is.null(matrixInverse)) 
  {
    message("Getting result from cached data");
    return(matrixInverse);
  }
  
  ## Else compute inverse of cached matrix, cache and return computed value.
  matrixInverse <- solve(x$get());
  x$setMatrixInverse(matrixInverse);
  
  return(matrixInverse);
}

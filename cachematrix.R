## The following functions calculate and cache the inverse of a matrix.


## makeCacheMatrix() takes a square invertible matrix as input and 
## returns a list of functions that do the following respectively:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse matrix
## 4. get the values of the inverse matrix

makeCacheMatrix <- function(InputMatrix = matrix()) {

	InverseMatrix <- NULL

	SetMatrix <- function(x) {
			InputMatrix <<- x
			InverseMatrix <<- NULL
	}
	
	GetMatrix <- function() InputMatrix

	SetInverse <- function(i) InverseMatrix <<- i	

	GetInverse <- function() InverseMatrix	
	
	list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, SetInverse = SetInverse, GetInverse = GetInverse)	

}


## cacheSolve() returns the inverse of the input matrix using the functions 
## of the list provided by the makeCacheMatrix() function

cacheSolve <- function(MatrixFunctionList, ...) {
        
		## retrieve the inverse from the list
		InverseMatrix <- MatrixFunctionList$GetInverse() 

		## check if the inverse has already been calculated and if so return from cache
        	if(!is.null(InverseMatrix)) {
            	message("getting cached data")
                	return(InverseMatrix)
        	}


		## calculate, cache and return the inverse matrix
        	data <- MatrixFunctionList$GetMatrix()
        	InverseMatrix <- solve(data, ...)
        	MatrixFunctionList$SetInverse(InverseMatrix)
        	InverseMatrix
}

		## Together, the functions cacheSolve and makeCacheMatrix compute and store a matrix inverse upon the first call.
		## On subsequent calls, they retrieve the cached inverse.
		
		## makeCacheMatrix accepts an invertible matrix argument and returns a list of four functions which may
		## be called to perform the following tasks within the function environment: set the matrix value, 
		## retrieve the matrix value, set the inverse matrix value, or retrieve the inverse matrix value.	
		
		
		makeCacheMatrix <- function(x = matrix()) {
			cacheinv <- NULL
			setMatrix <- function (y = matrix()){
				x <<- y 
				cacheinv <<- NULL	
			}
			getMatrix <- function() x
			setInverse <- function(inv) cacheinv <<-inv
			getInverse <- function() cacheinv
			list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
		}
		
		
		## cacheSolve takes the list of functions created by makeCacheMatrix as an argument.  If the inverse of the
		## matrix passed to makeCacheMatrix has already been computed, cacheSolve retrieves it from the cache;
		## otherwise, cacheSolve computes the inverse matrix and saves it using the setInverse function.
		
		cacheSolve <- function(x, ...) {
		        ## Return a matrix that is the inverse of 'x'
		        cacheinv <- x$getInverse()
		        if(!is.null(cacheinv)){
		        	message("retrieving cached inverse")
		        	return(cacheinv)
		        	}
		        	else{
		        	datamatrix <- x$getMatrix()
		        	print(datamatrix)
		        	cacheinv <- solve(datamatrix,...)
		        	x$setInverse(cacheinv)
		        	cacheinv
		        	}
		}

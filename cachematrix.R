## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## This function is checking if the matrix x has changed
## since the inverse of x was created. 
## inv_mat is the inversed matrix
## cache_mat is the backup of matrix x that was done last 
## time that we did the inversion and calculated the inv_mat
## When it returns TRUE, it means that we need to recalculate the inverse
## When it returns FALSE it means that we can just get the inverse from 
## the cache
#
makeCacheMatrix <- function(x = matrix(), cache_mat) 
{
        calc <- FALSE
        message ("printing cache_mat")
        print (cache_mat)

        if ((identical (x, cache_mat)) == FALSE)
        {
                calc <- TRUE
        }
        
        return (calc)
}


## Write a short comment describing this function

## This function returns a matrix (inv_mat) that is the inverse of 'x'
## determine if we need to invert the matrix
## or get it from cache?

cacheSolve <- function(x = matrix ()) 
{      
        if (!exists ("inv_mat"))
        {
                ## x has not yet been inverted. inv_mat does not exist
                ## Doing the matrix inversion
                
                message ("Calculating the inverted matrix (inv_mat) for the first time")
                inv_mat <- solve (x) ## generate inv_mat       
        }
        else
        {
                ## inv_mat exists.
                ## check to see if x has changed on not
                ## call makeCacheMatrix function to check if we
                ## need to read inv_mat from cache or not
                
                message ("printing cache_mat before calling sub Func")
                print (cache_mat)
                
                calculate_inv <- makeCacheMatrix (x, cache_mat)
                
                if (calculate_inv == TRUE)
                {
                ## x has changed. we need to calculate the inverse
                
                message ("x has changed. Recalculating the inverted matrix (inv_mat)")        
                        
                inv_mat <- solve (x) ## generate inv_mat       
                }
        }

        ## Save input matrix (x) in cache_mat so we can use it next time
        ## to check to see if we should get the inverse from the cache
        
        cache_mat <<- x 
        message ("printing cache_mat at the end of the main function")
        print (cache_mat)
        
        message ("printing inv_mat at the end of the main function")
        print (inv_mat)
        
        ### creates an identity matrix for x. It will be used to
        ### confirm that the inversion was done correctly
        
        mat_row <- nrow (x)
        my_identity <<- diag (mat_row) 

        message ("printing the identity matrix at the end of the main function")
        print (my_identity)
        
        return (inv_mat)
}

## Verification


x <- matrix (c (4, 2, 7, 6, 4, 2, 7, 6, 4), nrow = 3, ncol =3)
y <- matrix (c (4, 2, 7, 6, 4, 2, 7, 6, 4, 2, 7, 6), nrow = 4, ncol =4)
z <- matrix (c (4, 2, 7, 6), nrow = 2, ncol =2)


inv_mat <- cacheSolve (x)
inv_mat
x_mult_inv <- x %*% inv_mat 
x_mult_inv
identical (x_mult_inv, identity)


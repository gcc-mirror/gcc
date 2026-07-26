! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! PR 126386 - size calculation for allocation of the lhs for matmmul
! was wrong.  Original test case by Christoph Hofer.

PROGRAM reallocation_bug
    REAL(8), DIMENSION(:), ALLOCATABLE :: y
    REAL(8), DIMENSION(3,2) :: A
    real(8), dimension(3) :: x
    
    A(:,1) = [1,-2,3]
    A(:,2) = [-4,5,6]
    
    x =  [7,-8,9]

    ALLOCATE(y(3))
    y = matmul(transpose(2*a),x)
    if (size(y,1) /= 2) stop 1
    if (any(y /= [100, -28])) stop 2
    
END PROGRAM reallocation_bug

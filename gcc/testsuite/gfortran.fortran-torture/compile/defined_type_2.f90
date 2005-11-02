!This used to ICE as we chose the wrong type for the
! temporary to hold type%x
! fortran/18157
MODULE bug 
 IMPLICIT NONE 
 TYPE :: my_type 
   REAL :: x 
 END TYPE 
 TYPE (my_type), DIMENSION(3) :: t 
 CONTAINS 
   SUBROUTINE foo 
   INTEGER, DIMENSION(8)        :: c(3) 
   t(c)%x = t(c)%x 
   RETURN 
  END SUBROUTINE foo 
END MODULE bug 
 

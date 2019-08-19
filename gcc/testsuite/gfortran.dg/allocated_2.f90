! { dg-do compile }
program foo

   implicit none

   integer, allocatable :: x
   integer, allocatable :: a(:)

   logical a1, a2

   a1 = allocated(scalar=a)   ! { dg-error "Scalar entity required" }
   a2 = allocated(array=x)    ! { dg-error "Array entity required" }
   a1 = allocated(scalar=x, array=a)   ! { dg-error "Too many arguments" }
   a1 = allocated(array=a, scalar=x)   ! { dg-error "Too many arguments" }

end program foo

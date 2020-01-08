! { dg-do compile }
! PR fortran/91801
! Code contributed by Gerhard Steinmetz
program p
   integer, parameter :: a(2) = [2,0]
   print *, reshape([1,2,3,4,5,6], [2,3], order=a) ! { dg-error "Element with a value of 0 in ORDER at .1. must be in the range .1, ..., 2. for the RESHAPE intrinsic near .2." }
end

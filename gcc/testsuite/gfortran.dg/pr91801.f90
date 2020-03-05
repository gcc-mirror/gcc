! { dg-do compile }
! PR fortran/91801
! Code contributed by Gerhard Steinmetz
program p
   integer, parameter :: a(2) = [2,0]              ! { dg-error "Element with a value of" }
   print *, reshape([1,2,3,4,5,6], [2,3], order=a) ! { dg-error "for the RESHAPE intrinsic near" }
end

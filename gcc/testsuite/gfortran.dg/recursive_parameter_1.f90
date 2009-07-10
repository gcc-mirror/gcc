! { dg-do compile }
! Tests the fix for PR39334 in which the recursive parameter declaration
! caused a sgfault.
!
! Reported by James van Buskirk on comp.lang.fortran
!
program recursive_parameter
   implicit none
   integer, parameter :: dp = kind(1.0_dp) ! { dg-error "Missing kind-parameter" }
   write(*,*) dp ! { dg-error "has no IMPLICIT type" }
end program recursive_parameter

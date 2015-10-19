! { dg-do compile }
! Test the fix for pr56852, where an ICE would occur after the error.
!
! Contributed by Lorenz Huedepohl  <bugs@stellardeath.org>
!
program test
  implicit none
  real :: a(4)
  ! integer :: i
  read(0) (a(i),i=1,4) ! { dg-error "has no IMPLICIT type" }
end program

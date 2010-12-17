! { dg-do compile }
! Test for PR19925
!
program pr19925
  implicit none
  integer j
  integer, parameter :: n = 100000
  integer, parameter :: i(n)=(/(j,j=1,n)/) ! { dg-error "number of elements" }
  print *, i(5) ! { dg-error "has no IMPLICIT type" }
end program pr19925

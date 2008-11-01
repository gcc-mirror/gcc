! { dg-do compile }
! { dg-options "-fmax-array-constructor=100000" }
! Test for PR19925
!
program pr19925
  implicit none
  integer j
  integer, parameter :: n = 100000
  integer, parameter :: i(n) = (/ (j, j=1, n) /)
  print *, i(5)
end program pr19925

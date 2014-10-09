! { dg-do compile }
! PR 62142 - this used to segfault
! Original test case by Ondřej Čertík .
program test_segfault
  implicit none
  real, allocatable :: X(:)
  allocate (x(1))
  x = 1.
  X = floor(X)
end program

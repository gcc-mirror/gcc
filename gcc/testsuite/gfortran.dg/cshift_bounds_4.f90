! { dg-do run }
! { dg-shouldfail "Incorrect extent in SHIFT argument of CSHIFT intrinsic in dimension 1: is 3, should be 2" }
! { dg-options "-fbounds-check" }
program main
  integer, dimension(:,:), allocatable :: a, b
  integer, dimension(:), allocatable :: sh
  allocate (a(2,2))
  allocate (b(2,2))
  allocate (sh(3))
  a = 1
  b = cshift(a,sh)
end program main
! { dg-output "Fortran runtime error: Incorrect extent in SHIFT argument of CSHIFT intrinsic in dimension 1: is 3, should be 2" }

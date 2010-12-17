! { dg-do run }
! { dg-options "-fbounds-check -fno-realloc-lhs" }
! { dg-shouldfail "Incorrect extent in return value of CSHIFT intrinsic in dimension 2: is 3, should be 2" }
program main
  integer, dimension(:,:), allocatable :: a, b
  allocate (a(2,2))
  allocate (b(2,3))
  a = 1
  b = cshift(a,1)
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of CSHIFT intrinsic in dimension 2: is 3, should be 2" }

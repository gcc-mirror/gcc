! { dg-do run }
! { dg-options "-O -finline-matmul-limit=30 -fcheck=all" }
! { dg-shouldfail "Dimension of array B incorrect in MATMUL intrinsic" }
program main
  real, dimension(:,:), allocatable :: a
  real, dimension(:), allocatable :: b
  real, dimension(:), allocatable :: res
  allocate (a(2,2), b(3))
  call random_number(a)
  call random_number(b)
  res = matmul(a,b)
  print *,res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic in dimension 1: is 3, should be 2" }


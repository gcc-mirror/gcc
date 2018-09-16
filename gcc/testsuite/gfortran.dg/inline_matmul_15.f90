! { dg-do run }
! { dg-shouldfail "dimension of array B incorrect in MATMUL intrinsic" }
! { dg-options "-O -finline-matmul-limit=100 -fcheck=bounds" }
program main
  real, dimension(:,:), allocatable :: a
  real, dimension(:), allocatable :: b
  allocate (a(2,2), b(3))
  call random_number(a)
  call random_number(b)
  print *,matmul(a,b)
end program main
! { dg-output "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic in dimension 1" }

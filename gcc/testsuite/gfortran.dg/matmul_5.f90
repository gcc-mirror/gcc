! { dg-do run }
! { dg-shouldfail "dimension of array B incorrect in MATMUL intrinsic" }
program main
  real, dimension(:,:), allocatable :: a
  real, dimension(:), allocatable :: b
  allocate (a(2,2), b(3))
  call random_number(a)
  call random_number(b)
  print *,matmul(a,b)
end program main
! { dg-output "Fortran runtime error: dimension of array B incorrect in MATMUL intrinsic.*" }

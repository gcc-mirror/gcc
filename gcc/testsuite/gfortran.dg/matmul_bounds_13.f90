! { dg-do run }
! { dg-options "-fcheck=bounds" }
! { dg-shouldfail "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic in dimension 1" }
program main
  real, dimension(:,:), allocatable :: a, b, c
  character(len=100) :: line
  allocate (a(3,2))
  allocate (b(2,4))
  call random_number(a)
  call random_number(b)
  write (unit=line, fmt=*) matmul(a,transpose(b))
end program main
! { dg-output "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic in dimension 1" }

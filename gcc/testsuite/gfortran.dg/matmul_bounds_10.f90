! { dg-do run }
! { dg-options "-fno-backtrace -fbounds-check -fno-realloc-lhs" }
! { dg-shouldfail "Fortran runtime error: Incorrect extent in return array in MATMUL intrinsic for dimension 1: is 4, should be 3" }
program main
  real, dimension(3,2) :: a
  real, dimension(3,2) :: b
  real, dimension(:,:), allocatable :: ret
  allocate (ret(3,3))
  a = 1.0
  b = 2.3
  ret = matmul(a,transpose(b))  ! This is OK
  deallocate(ret)
  allocate(ret(4,3))
  ret = matmul(a,transpose(b))  ! This should throw an error.
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return array in MATMUL intrinsic for dimension 1: is 4, should be 3" }

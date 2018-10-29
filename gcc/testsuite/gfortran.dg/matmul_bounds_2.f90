! { dg-do run }
! { dg-options "-fbounds-check -fno-realloc-lhs" }
! { dg-shouldfail "Fortran runtime error: Incorrect extent in return array in MATMUL intrinsic for dimension 2: is 2, should be 3" }
program main
  real, dimension(3,2) :: a
  real, dimension(2,3) :: b
  real, dimension(:,:), allocatable :: ret
  allocate (ret(2,2))
  a = 1.0
  b = 2.3
  ret = matmul(b,a)  ! This is OK
  deallocate(ret)
  allocate(ret(3,2))
  ret = matmul(a,b)  ! This should throw an error.
end program main
! { dg-output "Fortran runtime error: Array bound mismatch for dimension 2 of array.*" }

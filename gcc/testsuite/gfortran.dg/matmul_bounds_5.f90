! { dg-do run }
! { dg-options "-fbounds-check -fno-realloc-lhs" }
! { dg-shouldfail "Fortran runtime error: Incorrect extent in return array in MATMUL intrinsic: is 3, should be 2" }
program main
  real, dimension(2,3) :: a
  real, dimension(3) :: b
  real, dimension(:), allocatable :: ret
  allocate (ret(2))
  a = 1.0
  b = 2.3
  ret = matmul(a,b)  ! This is OK
  deallocate(ret)
  allocate(ret(3))
  ret = matmul(a,b)  ! This should throw an error.
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return array in MATMUL intrinsic: is 3, should be 2" }

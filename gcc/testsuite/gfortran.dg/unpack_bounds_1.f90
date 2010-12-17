! { dg-do run }
! { dg-options "-fbounds-check -fno-realloc-lhs" }
! { dg-shouldfail "Incorrect extent in return value of UNPACK intrinsic in dimension 2: is 1, should be 2" }
program main
  integer, allocatable, dimension(:) :: vector
  integer, allocatable, dimension(:,:) :: res
  logical, allocatable, dimension(:,:) :: mask

  allocate (vector(2))
  allocate (mask(2,2))
  allocate (res(2,1))

  vector = 1
  mask = reshape((/ .TRUE., .FALSE., .FALSE., .TRUE. /),(/2,2/))
  res = unpack(vector, mask, 0)
  print *,res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of UNPACK intrinsic in dimension 2: is 1, should be 2" }

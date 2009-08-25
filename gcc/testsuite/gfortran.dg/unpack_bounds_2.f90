! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect size of return value in UNPACK intrinsic: should be at least 3, is 2" }
program main
  integer, allocatable, dimension(:) :: vector
  integer, allocatable, dimension(:,:) :: res
  logical, allocatable, dimension(:,:) :: mask

  allocate (vector(2))
  allocate (mask(2,2))
  allocate (res(2,2))

  vector = 1
  mask = reshape((/ .TRUE., .TRUE., .FALSE., .TRUE. /),(/2,2/))
  res = unpack(vector, mask, 0)
  print *,res
end program main
! { dg-output "Fortran runtime error: Incorrect size of return value in UNPACK intrinsic: should be at least 3, is 2" }

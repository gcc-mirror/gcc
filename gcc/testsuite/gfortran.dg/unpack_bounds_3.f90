! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect size of return value in UNPACK intrinsic: should be at least 3, is 2" }
program main
  integer, allocatable, dimension(:) :: vector
  integer, allocatable, dimension(:,:) :: res
  integer, allocatable, dimension(:,:) :: field
  logical, allocatable, dimension(:,:) :: mask

  allocate (vector(3))
  allocate (mask(2,2))
  allocate (res(2,2))
  allocate (field(3,2))

  vector = 1
  field = 0
  mask = reshape((/ .TRUE., .TRUE., .FALSE., .TRUE. /),(/2,2/))
  res = unpack(vector, mask, field)
  print *,res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in FIELD of UNPACK intrinsic in dimension 1: is 3, should be 2" }

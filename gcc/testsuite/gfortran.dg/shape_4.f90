! { dg-do run }
! PR 35001 - we need to return 0 for the shapes of
! negative extents.  Test case adapted from Tobias Burnus.
program main
  implicit none
  integer :: i,j, a(10,10),res(2)
  j = 1
  i = 10
  res = shape(a(1:1,i:j:1))
  if (res(1) /=1 .or. res(2) /= 0) call abort
  res = shape(a(1:1,j:i:-1))
  if (res(1) /=1 .or. res(2) /= 0) call abort
end program main

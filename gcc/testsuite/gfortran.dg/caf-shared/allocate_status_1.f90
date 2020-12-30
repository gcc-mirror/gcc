! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "4" }
program main
  integer, allocatable :: a[:]
  character (len=80) :: errmsg
  integer :: st
  st = 42
  allocate (a[*],stat=st)
  if (st /= 0) stop 1
  allocate (a[*], stat=st)
  if (st == 0) stop 1
  allocate (a[*], stat=st,errmsg=errmsg)
  if (st == 0) stop 2
  if (errmsg /= "Attempting to allocate already allocated variable") stop 3
end program main

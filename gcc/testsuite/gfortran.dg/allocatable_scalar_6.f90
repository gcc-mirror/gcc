! { dg-do run }
! { dg-options "-Wall -pedantic" }
!
! PR fortran/41872
!
!  (De)allocate tests
!
program test
  implicit none
  integer, allocatable :: a, b, c
  integer :: stat
  stat=99
  allocate(a, stat=stat)
  if (stat /= 0) STOP 1
  allocate(a, stat=stat)
  if (stat == 0) STOP 2

  allocate (b)
  deallocate (b, stat=stat)
  if (stat /= 0) STOP 3
  deallocate (b, stat=stat)
  if (stat == 0) STOP 4

  deallocate (c, stat=stat)
  if (stat == 0) STOP 5
end program test

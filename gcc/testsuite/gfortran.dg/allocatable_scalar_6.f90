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
  if (stat /= 0) call abort ()
  allocate(a, stat=stat)
  if (stat == 0) call abort ()

  allocate (b)
  deallocate (b, stat=stat)
  if (stat /= 0) call abort ()
  deallocate (b, stat=stat)
  if (stat == 0) call abort ()

  deallocate (c, stat=stat)
  if (stat == 0) call abort ()
end program test

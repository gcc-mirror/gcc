! { dg-do  run }
! { dg-options "-fdump-tree-original" }
! Test that minloc and maxloc using KINDs return the right
! kind, by using unformatted I/O for a specific kind.
program main
  implicit none
  real, dimension(3) :: a
  integer :: r1, r2, r4, r8
  integer :: k
  character(len=30) :: l1, l2

  ! Check via I/O if the KIND is used correctly
  a = [ 1.0, 3.0, 2.0]
  write (unit=l1,fmt=*) 2_1
  write (unit=l2,fmt=*) maxloc(a,kind=1)
  if (l1 /= l2) STOP 1

  write (unit=l1,fmt=*) 2_2
  write (unit=l2,fmt=*) maxloc(a,kind=2)
  if (l1 /= l2) STOP 2

  write (unit=l1,fmt=*) 2_4
  write (unit=l2,fmt=*) maxloc(a,kind=4)
  if (l1 /= l2) STOP 3

  write (unit=l1,fmt=*) 2_8
  write (unit=l2,fmt=*) maxloc(a,kind=8)
  if (l1 /= l2) STOP 4

  a = [ 3.0, -1.0, 2.0]

  write (unit=l1,fmt=*) 2_1
  write (unit=l2,fmt=*) minloc(a,kind=1)
  if (l1 /= l2) STOP 5

  write (unit=l1,fmt=*) 2_2
  write (unit=l2,fmt=*) minloc(a,kind=2)
  if (l1 /= l2) STOP 6

  write (unit=l1,fmt=*) 2_4
  write (unit=l2,fmt=*) minloc(a,kind=4)
  if (l1 /= l2) STOP 7

  write (unit=l1,fmt=*) 2_8
  write (unit=l2,fmt=*) minloc(a,kind=8)
  if (l1 /= l2) STOP 8

end program main

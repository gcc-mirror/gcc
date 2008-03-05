! { dg-do run { target fd_truncate } }
! { dg-options "-fconvert=swap" }
! PR 26735 - implied open didn't use to honor -fconvert
program main
  implicit none
  integer (kind=4) :: i1, i2, i3
  write (10) 1_4
  close (10)
  open (10, form="unformatted", access="direct", recl=4)
  read (10,rec=1) i1
  read (10,rec=2) i2
  read (10,rec=3) i3
  if (i1 /= 4 .or. i2 /= 1 .or. i3 /= 4) call abort
  close (10,status="delete")
end program main

! PR fortran/47878
! { dg-do run { target fd_truncate } }
  integer :: a(5)
  open (99, recl = 40)
  write (99, '(5i3)') 1, 2, 3
  rewind (99)
  read (99, '(5i3)') a
  if (any (a.ne.(/1, 2, 3, 0, 0/))) STOP 1
  close (99, status = 'delete')
end

! { dg-do run }
! PR119856, the error should occur in both write statements.
program badfmt
  implicit none

  character(10):: fmt = "(AI5)"  ! Not a PARAMETER so not examined
                                 ! at compile time
  integer :: ioerr
  ioerr = 0
  write (*, fmt, iostat=ioerr) 'value =', 42
  if (ioerr /= 5006) stop 10
!
  write (*, fmt, iostat=ioerr) 'value =', 43
  if (ioerr /= 5006) stop 13
end program badfmt

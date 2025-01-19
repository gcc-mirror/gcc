! { dg-do run }
! F2008 constraint C1002 requires comma between A and F in a format
program badfmt
  implicit none
  integer :: myerror = 0
  character(40):: fmt = "(AF9.6)"
  open (10, status='scratch')
  write (10,fmt, iostat=myerror) 'pi =',4*atan(1.0)
  if (myerror /= 5006) stop 1
end program badfmt

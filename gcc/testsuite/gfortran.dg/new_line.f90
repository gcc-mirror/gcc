! { dg-do run }
! Checks Fortran 2003's new_line intrinsic function
! PR fortran/28585
program new_line_check
  implicit none
  character(len=10) :: a1
  character(len=10) :: a2(2)
  character(len=10), parameter :: a3 = "1234567890"
  character(len=10), parameter :: a4(2) = "1234567890"
  character(len=10), parameter :: a5(2) = repeat("1234567890",2)

  if(achar(10) /= new_line('a')) STOP 1

  if (iachar(new_line(a1)) /= 10) STOP 2
  if (iachar(new_line(a2)) /= 10) STOP 3
  if (iachar(new_line(a3)) /= 10) STOP 4
  if (iachar(new_line(a4)) /= 10) STOP 5
  if (iachar(new_line(a5)) /= 10) STOP 6

end program new_line_check

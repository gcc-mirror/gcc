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

  if(achar(10) /= new_line('a')) call abort

  if (iachar(new_line(a1)) /= 10) call abort
  if (iachar(new_line(a2)) /= 10) call abort
  if (iachar(new_line(a3)) /= 10) call abort
  if (iachar(new_line(a4)) /= 10) call abort
  if (iachar(new_line(a5)) /= 10) call abort

end program new_line_check

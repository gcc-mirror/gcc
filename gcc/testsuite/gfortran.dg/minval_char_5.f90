! { dg-do  run }
! PR fortran/83316 - this used to ICE
program tminmaxval
  implicit none

  character(len=*), parameter :: b = "a"
  character(len=*), parameter :: e = "c"
  character(len=*), parameter :: s(3) = (/"a", "b", "c"/)

  if (minval(s) /= b) then
    STOP 1
  end if
  
  if (maxval(s) /= e) then
    STOP 2
  end if

end program tminmaxval

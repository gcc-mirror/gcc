! { dg-do  run }
! PR fortran/83316 - this used to ICE
program tminmaxval
  implicit none

  character(len=*), parameter :: b = "a"
  character(len=*), parameter :: e = "c"
  character(len=*), parameter :: s(3) = (/"a", "b", "c"/)

  if (minval(s) /= b) then
    call abort
  end if
  
  if (maxval(s) /= e) then
    call abort
  end if

end program tminmaxval

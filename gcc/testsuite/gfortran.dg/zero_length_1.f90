! { dg-do run }
! PR libfortran/31210
program test
  implicit none
  integer :: l = 0
  character(len=20) :: s
  
  write(s,'(A,I1)') foo(), 0
  if (trim(s) /= "0") STOP 1

contains

  function foo()
    character(len=l) :: foo
    foo = "XXXX"
  end function

end program test

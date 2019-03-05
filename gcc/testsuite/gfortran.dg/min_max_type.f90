! { dg-do  run }
! Make sure this is evaluated correctly even though max
! has been declared integer.
! Original test case by Gerhard Steinmetz.
program main
  integer :: max
  character(len=1), parameter :: c = max('a','b')
  character(len=1), parameter :: d = min('a','b')
  if (c /= 'b' .or. d /= 'a') stop 1
end program main

! { dg-do run }
! PR fortran/81116
! The assignment was broken due to a missing temporary.
! Original test case by Clive Page.

program test10
  implicit none
  character(:), allocatable :: string
  !
  string = '1234567890'
  string = string(1:5) // string(7:)
  if (string /= '123457890') STOP 1
end program test10

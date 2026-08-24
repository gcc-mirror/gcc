! { dg-do run }
! PR 49802
! character(len=*), value was rejected by gfortran despite being valid
! from Fortran 2008 onwards.  Verify that it compiles and that VALUE
! semantics are correct: modifications to the dummy do not affect the
! actual argument, and len() returns the actual argument's length.

program test
  implicit none
  character(len=10) :: str

  str = "123456789"
  call by_value (str)
  if (str /= "123456789") stop 1

contains

  subroutine by_value (y)
    character(len=*), value :: y
    if (len (y) /= 10) stop 2
    if (y /= "123456789 ") stop 3
    y = "abcdefghij"
    if (y /= "abcdefghij") stop 4
    ! str is accessible via host association; VALUE must not let
    ! the assignment to y propagate back.
    if (str /= "123456789") stop 5
  end subroutine

end program

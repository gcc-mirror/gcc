! { dg-do run }
! PR fortran/71203 - used to ICE on zero-length arrays or substrings
! Derived from original test cases by Gerhard Steinmetz

program p
  implicit none
  character(3), parameter :: a(4) = ' '
  character(*), parameter :: b(4) = 'abc'
  character(*), parameter :: x(*) = a(2:2)(3:1)
  character(*), parameter :: y(*) = a(2:1)(3:1)
  character(*), parameter :: z(*) = b(2:1)(2:3)
  if (size (x) /= 1 .or. len(x) /= 0) stop 1
  if (size (y) /= 0 .or. len(y) /= 0) stop 2
  if (size (z) /= 0 .or. len(z) /= 2) stop 3
end

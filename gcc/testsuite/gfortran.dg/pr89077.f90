! { dg-do run }
!
! PR fortran/89077 - ICE using * as len specifier for character parameter

program test
  implicit none
  integer :: i
  character(*), parameter :: s = 'abcdef'
  character(*), parameter :: t = transfer ([(s(i:i), i=1,len(s))], s)
  if (len (t) /= len (s) .or. t /= s) stop 1
end

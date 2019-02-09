! { dg-do run }
!
! Test fixes for substring simplications derived from
! PR fortran/89077 - ICE using * as len specifier for character parameter

program test
  implicit none
  integer :: i
  character(*), parameter :: s = 'abcdef', y = 'efcdab'
  character(6), save      :: t = transfer ([(s(i:i),  i=1,len(s)  )], s)
  character(*), parameter :: u = transfer ([(s(i:i+2),i=1,len(s),3)], s)
  character(6), save      :: v = transfer ([(s(i:i+2),i=1,len(s),3)], s)
  character(*), parameter :: w = transfer ([(y(i:i+1),i=len(s)-1,1,-2)], s)
  character(6), save      :: x = transfer ([(y(i:i+1),i=len(s)-1,1,-2)], s)
  if (len (t) /= len (s) .or. t /= s) stop 1
  if (len (u) /= len (s) .or. u /= s) stop 2
  if (len (v) /= len (s) .or. v /= s) stop 3
  if (len (w) /= len (s) .or. w /= s) stop 4
  if (len (x) /= len (s) .or. x /= s) stop 5
end

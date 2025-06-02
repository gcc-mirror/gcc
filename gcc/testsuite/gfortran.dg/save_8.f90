!{ dg-do run }

! Check PR120483 is fixed.
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!            and Peter Güntert  <peter@guentert.com> 

program save_8
  implicit none
  character(len=:), allocatable, save :: s1
  s1 = 'ABC'
  if (s1(3:3) /= 'C') stop 1
end program save_8


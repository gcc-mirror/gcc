! { dg-do run }
! Test the fix for PR47523 in which concatenations did not work
! correctly with assignments to deferred character length scalars.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
program main
  implicit none
  character(:), allocatable :: a, b
  a = 'a'
  if (a .ne. 'a') STOP 1
  a = a // 'x'
  if (a .ne. 'ax') STOP 2
  if (len (a) .ne. 2) STOP 3
  a = (a(2:2))
  if (a .ne. 'x') STOP 4
  if (len (a) .ne. 1) STOP 5
end program main

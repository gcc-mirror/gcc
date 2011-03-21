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
  if (a .ne. 'a') call abort
  a = a // 'x'
  if (a .ne. 'ax') call abort
  if (len (a) .ne. 2) call abort
  a = (a(2:2))
  if (a .ne. 'x') call abort
  if (len (a) .ne. 1) call abort
end program main

! { dg-do run }
! Test the fix for PR47348, in which the substring length
! in the array constructor at line 19 would be missed and
! the length of q used instead.
!
! Contributed by Thomas Koenig  <tkoenig@netcologne.de>
!
program main
  implicit none
  character(len = *), parameter :: fmt='(2(A,"|"))'
  character(len = *), parameter :: test='xyc|aec|'
  integer :: i
  character(len = 4) :: q
  character(len = 8) :: buffer
  q = 'xy'
  i = 2
  write (buffer, fmt) (/ trim(q), 'ae' /)//'c'
  if (buffer .ne. test) Call abort
  write (buffer, FMT) (/ q(1:i), 'ae' /)//'c'
  if (buffer .ne. test) Call abort
end program main

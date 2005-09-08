! PR 19928.  Check the use of constant substring indexes in a
! scalarization loop.
! { dg-do run }
program main
  implicit none
  character (len = 5), dimension (2) :: a
  character (len = 3), dimension (2) :: b
  a = (/ 'abcde', 'ghijk' /)
  b = a(:)(2:4)
  if (b(1) .ne. 'bcd' .or. b(2) .ne. 'hij') call abort
end program main

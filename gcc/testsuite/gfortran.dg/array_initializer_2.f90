! { dg-do run }
! Tests the fix for PR28496 in which initializer array constructors with
! a missing initial array index would cause an ICE.
!
! Test for the fix of the initializer array constructor part of PR29975
! was added later.  Here, the indexing would get in a mess if the array
! specification had a lower bound other than unity.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
! Based on original test case from Samir Nordin  <snordin_ng@yahoo.fr> 
!
  integer, dimension(3), parameter :: a=(/1,2,3/)
  integer, dimension(3), parameter :: b=(/a(:)/)
  integer, dimension(3,3), parameter :: c=reshape ((/(i, i = 1,9)/),(/3,3/))
  integer, dimension(2,3), parameter :: d=reshape ((/c(3:2:-1,:)/),(/2,3/))
  integer, dimension(3,3), parameter :: e=reshape ((/a(:),a(:)+3,a(:)+6/),(/3,3/))
  integer, dimension(2,3), parameter :: f=reshape ((/c(2:1:-1,:)/),(/2,3/))
  CHARACTER (LEN=1), DIMENSION(3:7),  PARAMETER :: g =  &
    (/ '+', '-', '*', '/', '^' /)
  CHARACTER (LEN=3) :: h = "A+C"
!
! PR28496
!
  if (any (b .ne. (/1,2,3/))) STOP 1
  if (any (reshape(d,(/6/)) .ne. (/3, 2, 6, 5, 9, 8/))) STOP 2 
  if (any (reshape(f,(/6/)) .ne. (/2, 1, 5, 4, 8, 7/))) STOP 3 
!
! PR29975
!
  IF (all(h(2:2) /= g(3:4))) STOP 4
end

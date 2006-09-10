! { dg-do run }
! Tests the fix for PR28496 in which initializer array constructors with
! a missing initial array index would cause an ICE.
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
  if (any (b .ne. (/1,2,3/))) call abort ()
  if (any (reshape(d,(/6/)) .ne. (/3, 2, 6, 5, 9, 8/))) call abort () 
  if (any (reshape(f,(/6/)) .ne. (/2, 1, 5, 4, 8, 7/))) call abort () 
end

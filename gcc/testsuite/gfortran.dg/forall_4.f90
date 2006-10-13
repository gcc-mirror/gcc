! { dg-do run }
! Tests the fix for PR25072, in which mask expressions
! that start with an internal or intrinsic function 
! reference would give a syntax error.
!
! The fix for PR28119 is tested as well; here, the forall
! statement could not be followed by another statement on
! the same line.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module foo
  integer, parameter :: n = 4
contains
  pure logical function foot (i)
    integer, intent(in) :: i
    foot = (i == 2) .or. (i == 3)
  end function foot
end module foo

  use foo
  integer :: i, a(n)
  logical :: s(n)
  s = (/(foot (i), i=1, n)/)

! Check that non-mask case is still OK and the fix for PR28119
  a = 0
  forall (i=1:n) a(i) = i ; if (any (a .ne. (/1,2,3,4/))) call abort ()

! Now a mask using a function with an explicit interface
! via use association.
  a = 0
  forall (i=1:n, foot (i)) a(i) = i
  if (any (a .ne. (/0,2,3,0/))) call abort ()

! Now an array variable mask
  a = 0
  forall (i=1:n, .not. s(i)) a(i) = i
  if (any (a .ne. (/1,0,0,4/))) call abort ()

! This was the PR - an internal function mask
  a = 0
  forall (i=1:n, t (i)) a(i) = i
  if (any (a .ne. (/0,2,0,4/))) call abort ()

! Check that an expression is OK - this also gave a syntax
! error
  a = 0
  forall (i=1:n, mod (i, 2) == 0) a(i) = i
  if (any (a .ne. (/0,2,0,4/))) call abort ()

! And that an expression that used to work is OK
  a = 0
  forall (i=1:n, s (i) .or. t(i)) a(i) = w (i)
  if (any (a .ne. (/0,3,2,1/))) call abort ()

contains
  pure logical function t(i)
    integer, intent(in) :: i
    t = (mod (i, 2) == 0)
  end function t
  pure integer function w(i)
    integer, intent(in) :: i
    w = 5 - i
  end function w
end
! { dg-final { cleanup-modules "foo" } }

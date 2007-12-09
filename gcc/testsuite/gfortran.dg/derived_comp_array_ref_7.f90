! { dg-do run }
! Check the fix for PR32129 #4 in which the argument 'vec(vy(i, :))' was
! incorrectly simplified, resulting in an ICE.
!
! Reported by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
program testCode
  implicit none
  type vec
    real, dimension(2) :: coords
  end type
  integer :: i
  real, dimension(2,2), parameter :: vy = reshape ((/1,2,3,4/),(/2,2/))
  i = 1
  if (any (foo(vec(vy(i, :))) /= vy(i, :))) call abort ()

contains

  function foo (xin)
    type(vec) :: xin
    real, dimension (2) :: foo
    intent(in)  xin
    foo = xin%coords
  end function
end program

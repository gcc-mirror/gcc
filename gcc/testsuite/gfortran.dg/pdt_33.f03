! { dg-do run }
!
! Test the fix for PR102003, where len parameters where not returned as constants.
!
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
program pr102003
  type pdt(n)
     integer, len     :: n = 8
     character(len=n) :: c
  end type pdt
  type(pdt(42)) :: p
  integer, parameter :: m = len (p% c)
  integer, parameter :: lm = p% c% len

  if (m /= 42) stop 1
  if (len (p% c) /= 42) stop 2
  if (lm /= 42) stop 3
  if (p% c% len /= 42) stop 4
end


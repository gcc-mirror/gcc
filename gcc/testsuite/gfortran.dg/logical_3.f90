! { dg-do compile }
! This checks the fix for PR30406.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!===============================================================

function f()
  logical(8) :: f
  f = .false._8
end function f

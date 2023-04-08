! { dg-do compile }
! { dg-additional-options "-Wno-pedantic" }
!
! Test from PR103258.  This used to ICE due to incorrectly marking the
! no-implicit-type error for n and m in the character length expression
! as already diagnosed during early resolution, when in fact errors are
! ignored in that parsing context.  We now expect the errors to be diagnosed
! at the point of the first use of each symbol.

subroutine s(n) ! { dg-error "Symbol 'n' .*has no IMPLICIT type" }
implicit none
character(n+m) :: c ! { dg-error "Symbol 'm' .*has no IMPLICIT type" }
entry e(m)
end

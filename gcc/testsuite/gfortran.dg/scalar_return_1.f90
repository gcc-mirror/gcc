! { dg-do compile }
! { dg-options "-std=legacy" }
!
! tests the fix for pr25082 in which the return of an array by a
! subroutine went undremarked.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
SUBROUTINE S1(*)
INTEGER :: a(2)
RETURN a ! { dg-error " requires a SCALAR" }
END SUBROUTINE S1

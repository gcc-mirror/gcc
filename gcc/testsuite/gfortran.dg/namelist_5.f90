! { dg-do compile }
! Tests the fix for PR25054 in which namelist objects with non-constant
! shape were allowed.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
SUBROUTINE S1(I)
 integer :: a,b(I)
 NAMELIST /NLIST/ a,b ! { dg-error "must have constant shape" }
 a=1 ; b=2
 write(6,NML=NLIST)
END SUBROUTINE S1
END
! { dg-do compile }
! Tests the fix for 25083, in which the compiler failed to detect that
! data variables in BLOCK DATA were not in COMMON.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
 BLOCK DATA D
  INTEGER I ! { dg-error "must be in COMMON" }
  DATA I /1/
 END BLOCK DATA
END

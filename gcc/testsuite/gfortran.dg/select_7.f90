! { dg-do compile }
! Tests the fix for PR25073 in which overlap in logical case
! expressions was permitted.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
LOGICAL :: L
SELECT CASE(L)
CASE(.true.)
CASE(.false.)
CASE(.true.) ! { dg-error "value in CASE statement is repeated" }
END SELECT
END

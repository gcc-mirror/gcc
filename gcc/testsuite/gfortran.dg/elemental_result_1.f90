! { dg-do compile }
! Tests the fix for PR20874 in which array valued elemental
! functions were permitted.
!
! Contributed by Joost VandeVondele  <jv244@cam.ac.uk>
!
MODULE Test
CONTAINS
  ELEMENTAL FUNCTION LL(I) ! { dg-error "must have a scalar result" }
    INTEGER, INTENT(IN) :: I
    INTEGER  :: LL(2)
  END FUNCTION LL
!
! This was already OK.
!
  ELEMENTAL FUNCTION MM(I)
    INTEGER, INTENT(IN) :: I
    INTEGER, pointer  :: MM ! { dg-error "conflicts with ELEMENTAL" }
  END FUNCTION MM
END MODULE Test
! { dg-final { cleanup-modules "test" } }

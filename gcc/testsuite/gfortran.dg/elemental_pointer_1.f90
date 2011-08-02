! { dg-do compile }
! Tests the fix for pr20875.
! Note 12.7.1 "For a function, the result shall be scalar and shall not have the POINTER attribute."
MODULE Test
CONTAINS
  ELEMENTAL FUNCTION LL(I)
    INTEGER, INTENT(IN) :: I
    INTEGER :: LL
    POINTER  :: LL ! { dg-error " POINTER attribute conflicts with ELEMENTAL attribute" }
  END FUNCTION LL
END MODULE Test

! { dg-final { cleanup-modules "test" } }

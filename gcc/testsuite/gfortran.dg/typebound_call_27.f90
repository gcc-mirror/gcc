! { dg-do compile }
!
! PR fortran/66257
! Check that typebound function calls are accepted as actual argument.
!
MODULE test_class
  IMPLICIT NONE
  PRIVATE
  PUBLIC:: test

  INTEGER, PARAMETER :: dp  = SELECTED_REAL_KIND(15)

  TYPE test
      PRIVATE
      CONTAINS
          PRIVATE
              PROCEDURE, PUBLIC:: E
              PROCEDURE, PUBLIC:: Om
  END TYPE test

CONTAINS

  ELEMENTAL FUNCTION E (self, a)
    IMPLICIT NONE
    CLASS(test), INTENT(IN):: self
    REAL(kind=dp), INTENT(IN):: a
    REAL(kind=dp):: E

    E = a
  END FUNCTION E

  ELEMENTAL FUNCTION Om (self, z)
    IMPLICIT NONE
    CLASS(test), INTENT(IN):: self
    REAL(kind=dp), INTENT(IN):: z
    REAL(kind=dp):: Om

    Om = self%E(self%E(z))
    Om = log10(self%E(z))
  END FUNCTION Om
END MODULE test_class

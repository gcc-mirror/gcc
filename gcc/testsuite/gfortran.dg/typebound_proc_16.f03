! { dg-do compile }
!
! PR 44549: [OOP][F2008] Type-bound procedure: bogus error from list after PROCEDURE
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>

MODULE rational_numbers
  IMPLICIT NONE
  PRIVATE
  TYPE,PUBLIC :: rational
    PRIVATE
    INTEGER n,d

    CONTAINS
    ! ordinary type-bound procedure
    PROCEDURE :: real => rat_to_real
    ! specific type-bound procedures for generic support
    PROCEDURE,PRIVATE :: rat_asgn_i, rat_plus_rat, rat_plus_i
    PROCEDURE,PRIVATE,PASS(b) :: i_plus_rat
    ! generic type-bound procedures
    GENERIC :: ASSIGNMENT(=) => rat_asgn_i
    GENERIC :: OPERATOR(+) => rat_plus_rat, rat_plus_i, i_plus_rat
  END TYPE
  CONTAINS
    ELEMENTAL REAL FUNCTION rat_to_real(this) RESULT(r)
      CLASS(rational),INTENT(IN) :: this
      r = REAL(this%n)/this%d
    END FUNCTION

    impure ELEMENTAL SUBROUTINE rat_asgn_i(a,b)
      CLASS(rational),INTENT(OUT) :: a
      INTEGER,INTENT(IN) :: b
      a%n = b
      a%d = 1
    END SUBROUTINE

    ELEMENTAL TYPE(rational) FUNCTION rat_plus_i(a,b) RESULT(r)
      CLASS(rational),INTENT(IN) :: a
      INTEGER,INTENT(IN) :: b
      r%n = a%n + b*a%d
      r%d = a%d
    END FUNCTION

    ELEMENTAL TYPE(rational) FUNCTION i_plus_rat(a,b) RESULT(r)
      INTEGER,INTENT(IN) :: a
      CLASS(rational),INTENT(IN) :: b
      r%n = b%n + a*b%d
      r%d = b%d
    END FUNCTION

    ELEMENTAL TYPE(rational) FUNCTION rat_plus_rat(a,b) RESULT(r)
      CLASS(rational),INTENT(IN) :: a,b
      r%n = a%n*b%d + b%n*a%d
      r%d = a%d*b%d
    END FUNCTION
END

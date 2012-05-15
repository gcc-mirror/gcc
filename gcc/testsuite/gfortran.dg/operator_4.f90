! PR 17711 : Verify error message text meets operator in source
! { dg-do compile }

MODULE mod_t
  type :: t
    integer :: x
  end type

  INTERFACE OPERATOR(==)
    MODULE PROCEDURE t_eq
  END INTERFACE

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE t_ne
  END INTERFACE

  INTERFACE OPERATOR(>)
    MODULE PROCEDURE t_gt
  END INTERFACE

  INTERFACE OPERATOR(>=)
    MODULE PROCEDURE t_ge
  END INTERFACE

  INTERFACE OPERATOR(<)
    MODULE PROCEDURE t_lt
  END INTERFACE

  INTERFACE OPERATOR(<=)
    MODULE PROCEDURE t_le
  END INTERFACE

CONTAINS
  LOGICAL FUNCTION t_eq(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_eq = (this%x == other%x)
  END FUNCTION

  LOGICAL FUNCTION t_ne(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_ne = (this%x /= other%x)
  END FUNCTION

  LOGICAL FUNCTION t_gt(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_gt = (this%x > other%x)
  END FUNCTION

  LOGICAL FUNCTION t_ge(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_ge = (this%x >= other%x)
  END FUNCTION

  LOGICAL FUNCTION t_lt(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_lt = (this%x < other%x)
  END FUNCTION

  LOGICAL FUNCTION t_le(this, other)
    TYPE(t), INTENT(in) :: this, other
    t_le = (this%x <= other%x)
  END FUNCTION
END MODULE

PROGRAM pr17711
  USE mod_t

  LOGICAL :: A
  INTEGER :: B
  TYPE(t) :: C

  A = (A == B)   ! { dg-error "comparison operator '=='" }
  A = (A.EQ.B)   ! { dg-error "comparison operator '.eq.'" }
  A = (A /= B)   ! { dg-error "comparison operator '/='" }
  A = (A.NE.B)   ! { dg-error "comparison operator '.ne.'" }
  A = (A <= B)   ! { dg-error "comparison operator '<='" }
  A = (A.LE.B)   ! { dg-error "comparison operator '.le.'" }
  A = (A <  B)   ! { dg-error "comparison operator '<'" }
  A = (A.LT.B)   ! { dg-error "comparison operator '.lt.'" }
  A = (A >= B)   ! { dg-error "comparison operator '>='" }
  A = (A.GE.B)   ! { dg-error "comparison operator '.ge.'" }
  A = (A >  B)   ! { dg-error "comparison operator '>'" }
  A = (A.GT.B)   ! { dg-error "comparison operator '.gt.'" }

  ! this should also work with user defined operators
  A = (A == C)   ! { dg-error "comparison operator '=='" }
  A = (A.EQ.C)   ! { dg-error "comparison operator '.eq.'" }
  A = (A /= C)   ! { dg-error "comparison operator '/='" }
  A = (A.NE.C)   ! { dg-error "comparison operator '.ne.'" }
  A = (A <= C)   ! { dg-error "comparison operator '<='" }
  A = (A.LE.C)   ! { dg-error "comparison operator '.le.'" }
  A = (A <  C)   ! { dg-error "comparison operator '<'" }
  A = (A.LT.C)   ! { dg-error "comparison operator '.lt.'" }
  A = (A >= C)   ! { dg-error "comparison operator '>='" }
  A = (A.GE.C)   ! { dg-error "comparison operator '.ge.'" }
  A = (A >  C)   ! { dg-error "comparison operator '>'" }
  A = (A.GT.C)   ! { dg-error "comparison operator '.gt.'" }
END PROGRAM

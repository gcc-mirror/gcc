! { dg-do compile }
! PR42684 (42680) Ice with Interface.
MODULE mod1
  IMPLICIT NONE  
  TYPE ta
    INTEGER i
  END TYPE ta
  INTERFACE OPERATOR(+)
    MODULE PROCEDURE add_a
  END INTERFACE OPERATOR(+)  
CONTAINS  
  FUNCTION add_a(lhs, rhs) RESULT(r)
    TYPE(ta), INTENT(IN) :: lhs
    TYPE(ta), INTENT(IN) :: rhs
    TYPE(ta) :: r
    !****
    r%i = lhs%i + rhs%i
  END FUNCTION add_a  
END MODULE mod1

MODULE mod2
  IMPLICIT NONE 
  TYPE tb
    INTEGER j
  END TYPE tb
  INTERFACE OPERATOR(+)
    MODULE PROCEDURE add_b
  END INTERFACE OPERATOR(+)  
CONTAINS  
  SUBROUTINE other_proc()
    USE mod1    ! Causes ICE
  END SUBROUTINE other_proc  
  FUNCTION add_b(lhs, rhs) RESULT(r)
    TYPE(tb), INTENT(IN) :: lhs
    TYPE(tb), INTENT(IN) :: rhs
    TYPE(tb) :: r
    !****
    r%j = lhs%j + rhs%j
  END FUNCTION add_b  
END MODULE mod2

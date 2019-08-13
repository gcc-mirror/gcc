! { dg-do compile }
! Code contributed by Ian Harvey  <ian_harvey at bigpond dot com>
  MODULE m1
    IMPLICIT NONE
    PUBLIC :: False
    PUBLIC :: True
  CONTAINS
    FUNCTION False() RESULT(b)
      LOGICAL :: b
      b = .FALSE.
    END FUNCTION False
    
    FUNCTION True() RESULT(b)
      LOGICAL :: b
      b = .TRUE.
    END FUNCTION True
  END MODULE m1

  MODULE m2
    USE m1
    IMPLICIT NONE
    TYPE, ABSTRACT :: t_parent
    CONTAINS
      PROCEDURE(False), DEFERRED, NOPASS :: Binding
    END TYPE t_parent
  CONTAINS
    SUBROUTINE s
      TYPE, EXTENDS(t_parent) :: t_extension
      CONTAINS
        PROCEDURE, NOPASS :: Binding => True
      END TYPE t_extension
    END SUBROUTINE s
  END MODULE m2

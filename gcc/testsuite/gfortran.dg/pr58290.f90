! PR ipa/58290
! { dg-do compile }
! { dg-options "-O1 -fipa-pta" }

MODULE pr58290
  TYPE b
    CHARACTER(10) :: s = ''
  END TYPE b
  TYPE c
    TYPE(b) :: d
  END TYPE c
  TYPE h
    INTEGER, DIMENSION(:), POINTER :: b
  END TYPE h
CONTAINS
  SUBROUTINE foo(x, y)
    LOGICAL, INTENT(IN) :: x
    TYPE(c), INTENT(INOUT) :: y
  END SUBROUTINE 
  FUNCTION bar (g) RESULT (z)
    TYPE(h), INTENT(IN) :: g
    TYPE(c) :: y
    CALL foo (.TRUE., y)
    z = SIZE (g%b)
  END FUNCTION bar
  SUBROUTINE baz (g)
    TYPE(h), INTENT(INOUT) :: g
    INTEGER :: i, j
    j = bar(g)
    DO i = 1, j
    ENDDO
  END SUBROUTINE baz
END MODULE

! PR tree-optimization/88964
! { dg-do compile }
! { dg-options "-O3 -fno-tree-forwprop --param sccvn-max-alias-queries-per-access=1" }

MODULE pr88964
  INTEGER, PARAMETER :: dp=8
  REAL(KIND=dp) :: p, q, o
CONTAINS
  SUBROUTINE foo(a,b,c,f,h)
    IMPLICIT NONE
    INTEGER :: a, b, c
    REAL(KIND=dp) :: f(b*c), h(a*c)
    CALL bar(h)
    CALL baz(f)
    CALL qux(h)
  END SUBROUTINE foo
  SUBROUTINE bar(h)
    IMPLICIT NONE
    REAL(KIND=dp) :: h(1*1)
    INTEGER :: r, s, t, u
    DO u = 1,3
      DO t = 1,1
        DO s = 1,3
          DO r = 1,1
            h((t-1)*1+r) = h((t-1)*1+r)-p*o
          END DO
        END DO
      END DO
    END DO
  END SUBROUTINE bar
  SUBROUTINE baz(f)
    IMPLICIT NONE
    REAL(KIND=dp) :: f(3*1)
    INTEGER :: s, t, u
    DO u = 1,4
      DO t = 1,1
        DO s = 1,3
          f((t-1)*3+s) = f((t-1)*3+s) - q
        END DO
      END DO
    END DO
  END SUBROUTINE baz
  SUBROUTINE qux(h)
    IMPLICIT NONE
    REAL(KIND=dp) :: h(1*1)
    INTEGER :: r, s, t, u
    DO u = 1,5
      DO t = 1,1
        DO s = 1,3
          DO r = 1,1
            h((t-1)*1+r) = h((t-1)*1+r)-p*o
          END DO
        END DO
      END DO
    END DO
  END SUBROUTINE qux
END MODULE pr88964

! { dg-do compile }
! Tests the fix for PR29652, in which ambiguous interfaces were not detected
! with more than two specific procedures in the interface.
!
! Contributed by Daniel Franke  <franke.daniel@gmail.com>
!
MODULE global
INTERFACE iface
  MODULE PROCEDURE sub_a
  MODULE PROCEDURE sub_b ! { dg-error "Ambiguous interfaces" }
  MODULE PROCEDURE sub_c
END INTERFACE
CONTAINS
  SUBROUTINE sub_a(x)
    INTEGER, INTENT(in) :: x
    WRITE (*,*) 'A: ', x
  END SUBROUTINE
  SUBROUTINE sub_b(y)
    INTEGER, INTENT(in) :: y
    WRITE (*,*) 'B: ', y
  END SUBROUTINE
  SUBROUTINE sub_c(x, y)
    REAL, INTENT(in) :: x, y
    WRITE(*,*) x, y
  END SUBROUTINE
END MODULE

! { dg-final { cleanup-modules "global" } }

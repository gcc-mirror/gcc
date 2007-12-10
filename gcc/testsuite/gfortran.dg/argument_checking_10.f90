! { dg-do compile }
!
! PR fortran/34425
!
! Contributed by Joost VandeVondele
!
IMPLICIT NONE
INTEGER :: i(-1:1)
INTEGER :: j(-2:-1)
CALL S(i)
CALL S(j) ! { dg-warning "Actual argument contains too few elements for dummy argument 'i' .2/3." }
CONTAINS
 SUBROUTINE S(i)
  INTEGER :: i(0:2)
 END SUBROUTINE
END

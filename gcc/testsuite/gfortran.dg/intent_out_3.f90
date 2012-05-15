! { dg-do compile }
!
! PR fortran/34662
! The INTENT error was not detected.
! Test case contributed by Joost VandeVondele.
!
MODULE M1
 TYPE T1
  INTEGER :: I(3)
 END TYPE T1
 TYPE(T1), PARAMETER :: D1=T1((/1,2,3/))
CONTAINS
 SUBROUTINE S1(J)
  INTEGER, INTENT(INOUT) :: J
 END SUBROUTINE S1
END MODULE M1
USE M1
CALL S1(D1%I(3)) ! { dg-error "variable definition context" }
END

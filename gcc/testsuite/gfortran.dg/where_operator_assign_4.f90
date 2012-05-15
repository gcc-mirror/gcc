! { dg-do compile }
! PR fortran/34661 ICE on user-defined assignments in where statements
! Testcase contributed by Joost VandeVondele

MODULE M1
 IMPLICIT NONE
 TYPE T1
   INTEGER :: I
 END TYPE T1
 INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE S1
 END INTERFACE
CONTAINS
 SUBROUTINE S1(I,J)
   TYPE(T1), INTENT(OUT)  :: I(2)
   TYPE(T1), INTENT(IN)  :: J(2)
   I%I=-J%I
 END SUBROUTINE S1
END MODULE M1

USE M1
TYPE(T1) :: I(2),J(2)
I(:)%I=1
WHERE (I(:)%I>0)
 J=I                       ! { dg-error "Non-ELEMENTAL user-defined assignment in WHERE" }
END WHERE

WHERE (I(:)%I>0) J=I       ! { dg-error "Non-ELEMENTAL user-defined assignment in WHERE" }

END

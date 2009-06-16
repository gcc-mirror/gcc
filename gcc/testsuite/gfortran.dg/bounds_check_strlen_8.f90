! { dg-do run }
! { dg-options "-fbounds-check" }
!
! PR fortran/40383
! Gave before a bogus out of bounds.
! Contributed by Joost VandeVondele.
!
MODULE M1
  INTEGER, PARAMETER :: default_string_length=80
END MODULE M1
MODULE M2
 USE M1
 IMPLICIT NONE
CONTAINS
 FUNCTION F1(a,b,c,d) RESULT(RES)
   CHARACTER(LEN=default_string_length), OPTIONAL :: a,b,c,d
   LOGICAL :: res
 END FUNCTION F1
END MODULE M2

MODULE M3
 USE M1
 USE M2
 IMPLICIT NONE
CONTAINS
 SUBROUTINE S1
   CHARACTER(LEN=default_string_length) :: a,b
   LOGICAL :: L1
   INTEGER :: i
   DO I=1,10
      L1=F1(a,b)
   ENDDO
 END SUBROUTINE
END MODULE M3

USE M3
CALL S1
END

! { dg-final { cleanup-modules "m1 m2 m3" } }

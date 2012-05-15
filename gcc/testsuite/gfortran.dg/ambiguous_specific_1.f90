! { dg-do compile }
! Checks the fix for PR33542, in which the ambiguity in the specific
! interfaces of foo was missed.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
MODULE M1
   INTERFACE FOO
     MODULE PROCEDURE FOO
   END INTERFACE
CONTAINS
   SUBROUTINE FOO(I)
     INTEGER, INTENT(IN) :: I
     WRITE(*,*) 'INTEGER'
   END SUBROUTINE FOO
END MODULE M1

MODULE M2
   INTERFACE FOO
     MODULE PROCEDURE FOO
   END INTERFACE
CONTAINS
   SUBROUTINE FOO(R)
     REAL, INTENT(IN) :: R
     WRITE(*,*) 'REAL'
   END SUBROUTINE FOO
END MODULE M2

PROGRAM P
   USE M1
   USE M2
   implicit none
   external bar
   CALL FOO(10)
   CALL FOO(10.)
   call bar (foo)  ! { dg-error "is ambiguous" }
END PROGRAM P

! { dg-do compile }
! Checks the fix for PR33542 does not throw an error if there is no
! ambiguity in the specific interfaces of foo.
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
     MODULE PROCEDURE FOOFOO
   END INTERFACE
CONTAINS
   SUBROUTINE FOOFOO(R)
     REAL, INTENT(IN) :: R
     WRITE(*,*) 'REAL'
   END SUBROUTINE FOOFOO
END MODULE M2

PROGRAM P
   USE M1
   USE M2
   implicit none
   external bar
   CALL FOO(10)
   CALL FOO(10.)
   call bar (foo) 
END PROGRAM P

SUBROUTINE bar (arg)
  EXTERNAL arg
END SUBROUTINE bar
! { dg-final { cleanup-modules "m1 m2" } }

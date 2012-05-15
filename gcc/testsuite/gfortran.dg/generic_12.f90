! { dg-do compile }
! Test the fix for PR30476 in which the generic interface hello
! was found incorrectly to be ambiguous.
!
!Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
SUBROUTINE hello_x(dum)
   IMPLICIT NONE
   INTEGER :: dum
   WRITE(0,*) "Hello world: ", dum
END SUBROUTINE hello_x

MODULE interfaces
IMPLICIT NONE
INTERFACE hello
   SUBROUTINE hello_x(dum)
      IMPLICIT NONE
      INTEGER :: dum
   END SUBROUTINE hello_x
END INTERFACE
END MODULE interfaces

MODULE global_module
  USE interfaces
END MODULE global_module

PROGRAM main
  USE global_module
  IMPLICIT NONE
  CALL hello(10)
END PROGRAM main

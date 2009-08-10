! { dg-do run }

! FIXME: Remove -w once switched to polymorphic passed-object dummy arguments.
! { dg-options "-w" }

! Type-bound procedures
! Check calls with GENERIC bindings.

MODULE m
  IMPLICIT NONE

  TYPE t
  CONTAINS
    PROCEDURE, NOPASS :: plain_int
    PROCEDURE, NOPASS :: plain_real
    PROCEDURE, PASS(me) :: passed_intint
    PROCEDURE, PASS(me) :: passed_realreal

    GENERIC :: gensub => plain_int, plain_real, passed_intint, passed_realreal
  END TYPE t

CONTAINS

  SUBROUTINE plain_int (x)
    IMPLICIT NONE
    INTEGER :: x
    WRITE (*,*) "Plain Integer"
  END SUBROUTINE plain_int

  SUBROUTINE plain_real (x)
    IMPLICIT NONE
    REAL :: x
    WRITE (*,*) "Plain Real"
  END SUBROUTINE plain_real

  SUBROUTINE passed_intint (me, x, y)
    IMPLICIT NONE
    CLASS(t) :: me
    INTEGER :: x, y
    WRITE (*,*) "Passed Integer"
  END SUBROUTINE passed_intint

  SUBROUTINE passed_realreal (x, me, y)
    IMPLICIT NONE
    REAL :: x, y
    CLASS(t) :: me
    WRITE (*,*) "Passed Real"
  END SUBROUTINE passed_realreal

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE

  TYPE(t) :: myobj

  CALL myobj%gensub (5)
  CALL myobj%gensub (2.5)
  CALL myobj%gensub (5, 5)
  CALL myobj%gensub (2.5, 2.5)
END PROGRAM main

! { dg-output "Plain Integer(\n|\r\n|\r).*Plain Real(\n|\r\n|\r).*Passed Integer(\n|\r\n|\r).*Passed Real" }
! { dg-final { cleanup-modules "m" } }

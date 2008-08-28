! { dg-do compile }

! FIXME: Remove -w after polymorphic entities are supported.
! { dg-options "-w" }

! Type-bound procedures
! Test that the basic syntax for specific bindings is parsed and resolved.

MODULE othermod
  IMPLICIT NONE

CONTAINS

  SUBROUTINE othersub ()
    IMPLICIT NONE
  END SUBROUTINE othersub

END MODULE othermod

MODULE testmod
  USE othermod
  IMPLICIT NONE

  TYPE t1
    ! Might be empty
  CONTAINS
    PROCEDURE proc1
    PROCEDURE, PASS(me) :: p2 => proc2
  END TYPE t1

  TYPE t2
    INTEGER :: x
  CONTAINS
    PRIVATE
    PROCEDURE, NOPASS, PRIVATE :: othersub
    PROCEDURE,NON_OVERRIDABLE,PUBLIC,PASS :: proc3
  END TYPE t2

  TYPE t3
  CONTAINS
    ! This might be empty for Fortran 2008
  END TYPE t3

  TYPE t4
  CONTAINS
    PRIVATE
    ! Empty, too
  END TYPE t4

CONTAINS
  
  SUBROUTINE proc1 (me)
    IMPLICIT NONE
    TYPE(t1) :: me
  END SUBROUTINE proc1

  REAL FUNCTION proc2 (x, me)
    IMPLICIT NONE
    REAL :: x
    TYPE(t1) :: me
    proc2 = x / 2
  END FUNCTION proc2

  INTEGER FUNCTION proc3 (me)
    IMPLICIT NONE
    TYPE(t2) :: me
    proc3 = 42
  END FUNCTION proc3

END MODULE testmod

! { dg-final { cleanup-modules "testmod" } }

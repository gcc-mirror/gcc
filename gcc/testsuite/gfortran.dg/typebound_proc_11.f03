! { dg-do compile }

! Type-bound procedures
! Test that legal usage of DEFERRED is accepted.

MODULE testmod
  IMPLICIT NONE

  ABSTRACT INTERFACE
    SUBROUTINE intf ()
    END SUBROUTINE intf
  END INTERFACE

  TYPE, ABSTRACT :: abstract_type
  CONTAINS
    PROCEDURE(intf), DEFERRED, NOPASS :: p1
    PROCEDURE(realproc), DEFERRED, NOPASS :: p2
  END TYPE abstract_type

  TYPE, EXTENDS(abstract_type) :: sub_type
  CONTAINS
    PROCEDURE, NOPASS :: p1 => realproc
    PROCEDURE, NOPASS :: p2 => realproc
  END TYPE sub_type

CONTAINS

  SUBROUTINE realproc ()
  END SUBROUTINE realproc

END MODULE testmod

! { dg-final { cleanup-modules "testmod" } }

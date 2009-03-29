! { dg-do compile }

! Type-bound procedures
! Test for resolution errors with DEFERRED, namely checks about invalid
! overriding and taking into account inherited DEFERRED bindings.
! Also check that DEFERRED attribute is saved to module correctly.

MODULE m1
  IMPLICIT NONE

  ABSTRACT INTERFACE
    SUBROUTINE intf ()
    END SUBROUTINE intf
  END INTERFACE

  TYPE, ABSTRACT :: abstract_type
  CONTAINS
    PROCEDURE(intf), DEFERRED, NOPASS :: def
    PROCEDURE, NOPASS :: nodef => realproc
  END TYPE abstract_type

CONTAINS

  SUBROUTINE realproc ()
  END SUBROUTINE realproc

END MODULE m1

MODULE m2
  USE m1
  IMPLICIT NONE

  TYPE, ABSTRACT, EXTENDS(abstract_type) :: sub_type1
  CONTAINS
    PROCEDURE(intf), DEFERRED, NOPASS :: nodef ! { dg-error "must not be DEFERRED" }
  END TYPE sub_type1

  TYPE, EXTENDS(abstract_type) :: sub_type2 ! { dg-error "must be ABSTRACT" }
  END TYPE sub_type2

END MODULE m2

! { dg-final { cleanup-modules "m1" } }

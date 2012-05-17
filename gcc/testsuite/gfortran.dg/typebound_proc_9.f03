! { dg-do compile }

! Type-bound procedures
! Test for basic parsing errors for invalid DEFERRED.

MODULE testmod
  IMPLICIT NONE

  ABSTRACT INTERFACE
    SUBROUTINE intf ()
    END SUBROUTINE intf
  END INTERFACE

  TYPE not_abstract
  CONTAINS
    PROCEDURE(intf), DEFERRED, NOPASS :: proc ! { dg-error "is not ABSTRACT" }
  END TYPE not_abstract

  TYPE, ABSTRACT :: abstract_type
  CONTAINS
    PROCEDURE, DEFERRED :: p2 ! { dg-error "Interface must be specified" }
    PROCEDURE(intf), NOPASS :: p3 ! { dg-error "should be declared DEFERRED" }
    PROCEDURE(intf), DEFERRED, NON_OVERRIDABLE :: p4 ! { dg-error "can't both" }
    PROCEDURE(unknown), DEFERRED :: p5 ! { dg-error "has no IMPLICIT|module procedure" }
    PROCEDURE(intf), DEFERRED, DEFERRED :: p6 ! { dg-error "Duplicate DEFERRED" }
    PROCEDURE(intf), DEFERRED :: p6 => proc ! { dg-error "is invalid for DEFERRED" }
    PROCEDURE(), DEFERRED :: p7 ! { dg-error "Interface-name expected" }
    PROCEDURE(intf, DEFERRED) :: p8 ! { dg-error "'\\)' expected" }
  END TYPE abstract_type

END MODULE testmod

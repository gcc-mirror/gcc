! { dg-do compile }
! Test the patch for PR29992. The standard requires that a
! module procedure be contained in the same scope as the
! interface or is use associated to it(12.3.2.1).
!
! Contributed by Daniel Franke  <franke.daniel@gmail.com>
!
MODULE class_foo_type
  TYPE :: foo
    INTEGER :: dummy
  END TYPE
contains
  SUBROUTINE bar_init_set_int(this, value)
    TYPE(foo), INTENT(out) :: this
    integer, intent(in) :: value
    this%dummy = value
  END SUBROUTINE
END MODULE

MODULE class_foo
USE class_foo_type, ONLY: foo, bar_init_set_int

INTERFACE foo_init
  MODULE PROCEDURE foo_init_default  ! { dg-error "is not a module procedure" }
END INTERFACE

INTERFACE bar_init
  MODULE PROCEDURE bar_init_default, bar_init_set_int  ! These are OK
END INTERFACE

INTERFACE
  SUBROUTINE foo_init_default(this)
    USE class_foo_type, ONLY: foo
    TYPE(foo), INTENT(out) :: this
  END SUBROUTINE
END INTERFACE

contains
  SUBROUTINE bar_init_default(this)
    TYPE(foo), INTENT(out) :: this
    this%dummy = 42
  END SUBROUTINE

END MODULE

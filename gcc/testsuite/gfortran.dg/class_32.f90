! { dg-do compile }
!
! PR 45827: [4.6 Regression] [OOP] mio_component_ref(): Component not found
!
! Contributed by Daniel Franke <dfranke@gcc.gnu.org>

MODULE m

  TYPE, ABSTRACT :: t
    PRIVATE
    INTEGER   :: n
  CONTAINS
    PROCEDURE :: get
  END TYPE

  ABSTRACT INTERFACE
    SUBROUTINE create(this)
      IMPORT t
      CLASS(t) :: this
    END SUBROUTINE
  END INTERFACE

CONTAINS

  FUNCTION get(this)
    CLASS(t) :: this
    REAL, DIMENSION(this%n) :: get
  END FUNCTION

  SUBROUTINE destroy(this)
    CLASS(t) :: this
  END SUBROUTINE

END MODULE


PROGRAM p
  USE m
END

! { dg-final { cleanup-modules "m" } }

! { dg-do compile }
! Tests the fix for a further regression caused by the
! fix for PR28788, as noted in reply #13 in the Bugzilla
! entry by Martin Tee  <aovb94@dsl.pipex.com>.
! The problem was caused by contained, use associated
! derived types with pointer components of a derived type
! use associated in a sibling procedure, where both are
! associated by an ONLY clause. This is the reporter's
! test case.
!
MODULE type_mod
  TYPE a
    INTEGER  :: n(10)
  END TYPE a

  TYPE b
    TYPE (a), POINTER :: m(:) => NULL ()
  END TYPE b
END MODULE type_mod

MODULE seg_mod
CONTAINS
  SUBROUTINE foo (x)
    USE type_mod, ONLY : a     ! failed
    IMPLICIT NONE
    TYPE (a)  :: x
    RETURN
  END SUBROUTINE foo

  SUBROUTINE bar (x)
    USE type_mod, ONLY : b     ! failed
    IMPLICIT NONE
    TYPE (b)  :: x
    RETURN
  END SUBROUTINE bar
END MODULE seg_mod
! { dg-final { cleanup-modules "type_mod seg_mod" } }

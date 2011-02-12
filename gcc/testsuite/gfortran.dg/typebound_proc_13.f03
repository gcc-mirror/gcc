! { dg-do compile }

! PR fortran/41177
! Test for additional errors with type-bound procedure bindings.
! Namely that non-scalar base objects are rejected for TBP calls which are
! NOPASS, and that passed-object dummy arguments must be scalar, non-POINTER
! and non-ALLOCATABLE.

MODULE m
  IMPLICIT NONE

  TYPE t
  CONTAINS
    PROCEDURE, NOPASS :: myproc
  END TYPE t

  TYPE t2
  CONTAINS
! FIXME: uncomment and dejagnuify once class arrays are enabled
!    PROCEDURE, PASS :: nonscalar ! { "must be scalar" }
    PROCEDURE, PASS :: is_pointer ! { dg-error "must not be POINTER" }
    PROCEDURE, PASS :: is_allocatable ! { dg-error "must not be ALLOCATABLE" }
  END TYPE t2

CONTAINS

  SUBROUTINE myproc ()
  END SUBROUTINE myproc

!  SUBROUTINE nonscalar (me)
!    CLASS(t2), INTENT(IN) :: me(:)
!  END SUBROUTINE nonscalar

  SUBROUTINE is_pointer (me)
    CLASS(t2), POINTER, INTENT(IN) :: me
  END SUBROUTINE is_pointer

  SUBROUTINE is_allocatable (me)
    CLASS(t2), ALLOCATABLE, INTENT(IN) :: me
  END SUBROUTINE is_allocatable

  SUBROUTINE test ()
    TYPE(t) :: arr(2)
    CALL arr%myproc () ! { dg-error "must be scalar" }
  END SUBROUTINE test

END MODULE m

! { dg-final { cleanup-modules "m" } }

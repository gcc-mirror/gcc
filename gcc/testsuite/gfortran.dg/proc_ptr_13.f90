! { dg-do compile }
! { dg-options "-g" }
!
! PR 38152: Procedure pointers as module variables.
!
! Contributed by Daniel Kraft <domob@gcc.gnu.org>

MODULE myfortran_binding

  IMPLICIT NONE
  PROCEDURE(error_stop), POINTER :: error_handler

CONTAINS

  LOGICAL FUNCTION myfortran_shutdown ()
    CALL error_handler ()
  END FUNCTION myfortran_shutdown

  SUBROUTINE error_stop ()
  END SUBROUTINE error_stop

END MODULE myfortran_binding


use myfortran_binding
external foo
error_handler => foo
end

! { dg-final { cleanup-modules "myfortran_binding" } }

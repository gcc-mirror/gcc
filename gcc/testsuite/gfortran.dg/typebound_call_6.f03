! { dg-do run }
! { dg-output "Super(\n|\r\n|\r).*Sub" }

! Type-bound procedures
! Check for calling right overloaded procedure.

MODULE m
  IMPLICIT NONE

  TYPE supert
  CONTAINS
    PROCEDURE, NOPASS :: proc => proc_super
  END TYPE supert

  TYPE, EXTENDS(supert) :: subt
  CONTAINS
    PROCEDURE, NOPASS :: proc => proc_sub
  END TYPE subt

CONTAINS

  SUBROUTINE proc_super ()
    IMPLICIT NONE
    WRITE (*,*) "Super"
  END SUBROUTINE proc_super

  SUBROUTINE proc_sub ()
    IMPLICIT NONE
    WRITE (*,*) "Sub"
  END SUBROUTINE proc_sub

END MODULE m

PROGRAM main
  USE m
  IMPLICIT NONE

  TYPE(supert) :: super
  TYPE(subt) :: sub

  CALL super%proc
  CALL sub%proc
END PROGRAM main

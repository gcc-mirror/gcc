! { dg-do compile }

! FIXME: Remove -w after polymorphic entities are supported.
! { dg-options "-w" }

! Type-bound procedures
! Test for errors in specific bindings, during resolution.

MODULE othermod
  IMPLICIT NONE
CONTAINS

  REAL FUNCTION proc_noarg ()
    IMPLICIT NONE
  END FUNCTION proc_noarg

END MODULE othermod

MODULE testmod
  USE othermod
  IMPLICIT NONE

  INTEGER :: noproc

  PROCEDURE() :: proc_nointf

  INTERFACE
    SUBROUTINE proc_intf ()
    END SUBROUTINE proc_intf
  END INTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE proc_abstract_intf ()
    END SUBROUTINE proc_abstract_intf
  END INTERFACE

  TYPE supert
  CONTAINS
    PROCEDURE, NOPASS :: super_overrid => proc_sub_noarg
    PROCEDURE, NOPASS, NON_OVERRIDABLE :: super_nonoverrid => proc_sub_noarg
  END TYPE supert

  TYPE, EXTENDS(supert) :: t
  CONTAINS

    ! Bindings that should succeed
    PROCEDURE, NOPASS :: p0 => proc_noarg
    PROCEDURE, PASS :: p1 => proc_arg_first
    PROCEDURE proc_arg_first
    PROCEDURE, PASS(me) :: p2 => proc_arg_middle
    PROCEDURE, PASS(me), NON_OVERRIDABLE :: p3 => proc_arg_last
    PROCEDURE, NOPASS :: p4 => proc_nome
    PROCEDURE, NOPASS :: p5 => proc_intf
    PROCEDURE, NOPASS :: super_overrid => proc_sub_noarg

    ! Bindings that should not succeed
    PROCEDURE :: e0 => undefined ! { dg-error "has no IMPLICIT|module procedure" }
    PROCEDURE, PASS :: e1 => proc_noarg ! { dg-error "at least one argument" }
    PROCEDURE :: e2 => proc_noarg ! { dg-error "at least one argument" }
    PROCEDURE, PASS(me) :: e3 => proc_nome ! { dg-error "no argument 'me'" }
    PROCEDURE, PASS(me) :: e4 => proc_mewrong ! { dg-error "of the derived" }
    PROCEDURE, PASS :: e5 => proc_mewrong ! { dg-error "of the derived" }
    PROCEDURE :: e6 => noproc ! { dg-error "module procedure" }
    PROCEDURE :: e7 => proc_nointf ! { dg-error "explicit interface" }
    PROCEDURE, NOPASS :: e8 => proc_abstract_intf ! { dg-error "explicit interface" }
    PROCEDURE :: super_nonoverrid => proc_arg_first ! { dg-error "NON_OVERRIDABLE" }

  END TYPE t

CONTAINS

  SUBROUTINE proc_arg_first (me, x)
    IMPLICIT NONE
    CLASS(t) :: me
    REAL :: x
  END SUBROUTINE proc_arg_first

  INTEGER FUNCTION proc_arg_middle (x, me, y)
    IMPLICIT NONE
    REAL :: x, y
    CLASS(t) :: me
  END FUNCTION proc_arg_middle

  SUBROUTINE proc_arg_last (x, me)
    IMPLICIT NONE
    CLASS(t) :: me
    REAL :: x
  END SUBROUTINE proc_arg_last

  SUBROUTINE proc_nome (arg, x, y)
    IMPLICIT NONE
    TYPE(t) :: arg
    REAL :: x, y
  END SUBROUTINE proc_nome

  SUBROUTINE proc_mewrong (me, x)
    IMPLICIT NONE
    REAL :: x
    INTEGER :: me
  END SUBROUTINE proc_mewrong

  SUBROUTINE proc_sub_noarg ()
  END SUBROUTINE proc_sub_noarg

END MODULE testmod

PROGRAM main
  IMPLICIT NONE

  TYPE t
  CONTAINS
    PROCEDURE, NOPASS :: proc_no_module ! { dg-error "module procedure" }
  END TYPE t

CONTAINS

  SUBROUTINE proc_no_module ()
  END SUBROUTINE proc_no_module

END PROGRAM main

! { dg-final { cleanup-modules "othermod testmod" } }

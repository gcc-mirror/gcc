! { dg-do compile }

! FIXME: Remove -w after polymorphic entities are supported.
! { dg-options "-w" }

! Type-bound procedures
! Test for the check if overriding methods "match" the overridden ones by their
! characteristics.

MODULE testmod
  IMPLICIT NONE

  TYPE supert
  CONTAINS

    ! For checking the PURE/ELEMENTAL matching.
    PROCEDURE, NOPASS :: pure1 => proc_pure
    PROCEDURE, NOPASS :: pure2 => proc_pure
    PROCEDURE, NOPASS :: nonpure => proc_sub
    PROCEDURE, NOPASS :: elemental1 => proc_elemental
    PROCEDURE, NOPASS :: elemental2 => proc_elemental
    PROCEDURE, NOPASS :: nonelem1 => proc_nonelem
    PROCEDURE, NOPASS :: nonelem2 => proc_nonelem

    ! Same number of arguments!
    PROCEDURE, NOPASS :: three_args_1 => proc_threearg
    PROCEDURE, NOPASS :: three_args_2 => proc_threearg

    ! For SUBROUTINE/FUNCTION/result checking.
    PROCEDURE, NOPASS :: subroutine1 => proc_sub
    PROCEDURE, NOPASS :: subroutine2 => proc_sub
    PROCEDURE, NOPASS :: intfunction1 => proc_intfunc
    PROCEDURE, NOPASS :: intfunction2 => proc_intfunc
    PROCEDURE, NOPASS :: intfunction3 => proc_intfunc

    ! For access-based checks.
    PROCEDURE, NOPASS, PRIVATE :: priv => proc_sub
    PROCEDURE, NOPASS, PUBLIC :: publ1 => proc_sub
    PROCEDURE, NOPASS, PUBLIC :: publ2 => proc_sub

    ! For passed-object dummy argument checks.
    PROCEDURE, NOPASS :: nopass1 => proc_stme1
    PROCEDURE, NOPASS :: nopass2 => proc_stme1
    PROCEDURE, PASS :: pass1 => proc_stme1
    PROCEDURE, PASS(me) :: pass2 => proc_stme1
    PROCEDURE, PASS(me1) :: pass3 => proc_stmeme

    ! For corresponding dummy arguments.
    PROCEDURE, PASS :: corresp1 => proc_stmeint
    PROCEDURE, PASS :: corresp2 => proc_stmeint
    PROCEDURE, PASS :: corresp3 => proc_stmeint

  END TYPE supert

  ! Checking for NON_OVERRIDABLE is in typebound_proc_5.f03.

  TYPE, EXTENDS(supert) :: t
  CONTAINS

    ! For checking the PURE/ELEMENTAL matching.
    PROCEDURE, NOPASS :: pure1 => proc_pure ! Ok, both pure.
    PROCEDURE, NOPASS :: pure2 => proc_sub ! { dg-error "must also be PURE" }
    PROCEDURE, NOPASS :: nonpure => proc_pure ! Ok, overridden not pure.
    PROCEDURE, NOPASS :: elemental1 => proc_elemental ! Ok, both elemental.
    PROCEDURE, NOPASS :: elemental2 => proc_nonelem ! { dg-error "must also be ELEMENTAL" }
    PROCEDURE, NOPASS :: nonelem1 => proc_nonelem ! Ok, non elemental.
    PROCEDURE, NOPASS :: nonelem2 => proc_elemental ! { dg-error "must not be ELEMENTAL" }

    ! Same number of arguments!
    PROCEDURE, NOPASS :: three_args_1 => proc_threearg ! Ok.
    PROCEDURE, NOPASS :: three_args_2 => proc_twoarg ! { dg-error "same number of formal arguments" }

    ! For SUBROUTINE/FUNCTION/result checking.
    PROCEDURE, NOPASS :: subroutine1 => proc_sub ! Ok, both subroutines.
    PROCEDURE, NOPASS :: subroutine2 => proc_intfunc ! { dg-error "must also be a SUBROUTINE" }
    PROCEDURE, NOPASS :: intfunction1 => proc_intfunc ! Ok, matching functions.
    PROCEDURE, NOPASS :: intfunction2 => proc_sub ! { dg-error "must also be a FUNCTION" }
    PROCEDURE, NOPASS :: intfunction3 => proc_realfunc ! { dg-error "matching result types" }

    ! For access-based checks.
    PROCEDURE, NOPASS, PUBLIC :: priv => proc_sub ! Ok, increases visibility.
    PROCEDURE, NOPASS, PUBLIC :: publ1 => proc_sub ! Ok, both PUBLIC.
    PROCEDURE, NOPASS, PRIVATE :: publ2 => proc_sub ! { dg-error "must not be PRIVATE" }

    ! For passed-object dummy argument checks.
    PROCEDURE, NOPASS :: nopass1 => proc_stme1 ! Ok, both NOPASS.
    PROCEDURE, PASS :: nopass2 => proc_tme1 ! { dg-error "must also be NOPASS" }
    PROCEDURE, PASS :: pass1 => proc_tme1 ! Ok.
    PROCEDURE, NOPASS :: pass2 => proc_stme1 ! { dg-error "must also be PASS" }
    PROCEDURE, PASS(me2) :: pass3 => proc_tmeme ! { dg-error "same position" }

    ! For corresponding dummy arguments.
    PROCEDURE, PASS :: corresp1 => proc_tmeint ! Ok.
    PROCEDURE, PASS :: corresp2 => proc_tmeintx ! { dg-error "should be named 'a'" }
    PROCEDURE, PASS :: corresp3 => proc_tmereal ! { dg-error "Types mismatch for dummy argument 'a'" }

  END TYPE t

CONTAINS

  PURE SUBROUTINE proc_pure ()
  END SUBROUTINE proc_pure

  ELEMENTAL SUBROUTINE proc_elemental (arg)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: arg
  END SUBROUTINE proc_elemental

  SUBROUTINE proc_nonelem (arg)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: arg
  END SUBROUTINE proc_nonelem

  SUBROUTINE proc_threearg (a, b, c)
    IMPLICIT NONE
    INTEGER :: a, b, c
  END SUBROUTINE proc_threearg

  SUBROUTINE proc_twoarg (a, b)
    IMPLICIT NONE
    INTEGER :: a, b
  END SUBROUTINE proc_twoarg

  SUBROUTINE proc_sub ()
  END SUBROUTINE proc_sub

  INTEGER FUNCTION proc_intfunc ()
    proc_intfunc = 42
  END FUNCTION proc_intfunc

  REAL FUNCTION proc_realfunc ()
    proc_realfunc = 42.0
  END FUNCTION proc_realfunc

  SUBROUTINE proc_stme1 (me, a)
    IMPLICIT NONE
    CLASS(supert) :: me
    INTEGER :: a
  END SUBROUTINE proc_stme1

  SUBROUTINE proc_tme1 (me, a)
    IMPLICIT NONE
    CLASS(t) :: me
    INTEGER :: a
  END SUBROUTINE proc_tme1

  SUBROUTINE proc_stmeme (me1, me2)
    IMPLICIT NONE
    CLASS(supert) :: me1, me2
  END SUBROUTINE proc_stmeme

  SUBROUTINE proc_tmeme (me1, me2)
    IMPLICIT NONE
    CLASS(t) :: me1, me2
  END SUBROUTINE proc_tmeme

  SUBROUTINE proc_stmeint (me, a)
    IMPLICIT NONE
    CLASS(supert) :: me
    INTEGER :: a
  END SUBROUTINE proc_stmeint

  SUBROUTINE proc_tmeint (me, a)
    IMPLICIT NONE
    CLASS(t) :: me
    INTEGER :: a
  END SUBROUTINE proc_tmeint

  SUBROUTINE proc_tmeintx (me, x)
    IMPLICIT NONE
    CLASS(t) :: me
    INTEGER :: x
  END SUBROUTINE proc_tmeintx

  SUBROUTINE proc_tmereal (me, a)
    IMPLICIT NONE
    CLASS(t) :: me
    REAL :: a
  END SUBROUTINE proc_tmereal

END MODULE testmod

! { dg-final { cleanup-modules "testmod" } }

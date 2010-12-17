! { dg-do compile }

! Type-bound procedures
! Compiling and errors with GENERIC binding declarations.
! Bindings with NOPASS.

MODULE m
  IMPLICIT NONE

  TYPE somet
  CONTAINS
    PROCEDURE, NOPASS :: p1 => intf1
    PROCEDURE, NOPASS :: p1a => intf1a
    PROCEDURE, NOPASS :: p2 => intf2
    PROCEDURE, NOPASS :: p3 => intf3
    PROCEDURE, NOPASS :: subr

    GENERIC :: gen1 => p1a ! { dg-error "are ambiguous" }

    GENERIC, PUBLIC :: gen1 => p1, p2
    GENERIC :: gen1 => p3 ! Implicitly PUBLIC.
    GENERIC, PRIVATE :: gen2 => p1

    GENERIC :: gen2 => p2 ! { dg-error "same access" }
    GENERIC :: gen1 => p1 ! { dg-error "already defined as specific binding" }
    GENERIC, PASS :: gen3 => p1 ! { dg-error "Expected access-specifier" }
    GENERIC :: p1 => p1 ! { dg-error "already a non-generic procedure" }
    PROCEDURE, NOPASS :: gen1 => intf1 ! { dg-error "already a procedure" }
    GENERIC :: gen3 => ! { dg-error "specific binding" }
    GENERIC :: gen4 => p1 x ! { dg-error "Junk after" }
    GENERIC :: gen5 => p_notthere ! { dg-error "Undefined specific binding" }
    GENERIC :: gen6 => p1
    GENERIC :: gen7 => gen6 ! { dg-error "must target a specific binding" }

    GENERIC :: gensubr => p2 ! { dg-error "mixed FUNCTION/SUBROUTINE" }
    GENERIC :: gensubr => subr

  END TYPE somet

  TYPE supert
  CONTAINS
    PROCEDURE, NOPASS :: p1 => intf1
    PROCEDURE, NOPASS :: p1a => intf1a
    PROCEDURE, NOPASS :: p2 => intf2
    PROCEDURE, NOPASS :: p3 => intf3
    PROCEDURE, NOPASS :: sub1 => subr

    GENERIC :: gen1 => p1, p2
    GENERIC :: gen1 => p3
    GENERIC :: gen2 => p1
    GENERIC :: gensub => sub1
  END TYPE supert

  TYPE, EXTENDS(supert) :: t
  CONTAINS
    GENERIC :: gen2 => p1a ! { dg-error "are ambiguous" }
    GENERIC :: gen2 => p3
    GENERIC :: p1 => p2 ! { dg-error "can't overwrite specific" }
    GENERIC :: gensub => p2 ! { dg-error "mixed FUNCTION/SUBROUTINE" }

    PROCEDURE, NOPASS :: gen1 => intf1 ! { dg-error "Can't overwrite GENERIC" }
  END TYPE t

CONTAINS

  INTEGER FUNCTION intf1 (a, b)
    IMPLICIT NONE
    INTEGER :: a, b
    intf1 = 42
  END FUNCTION intf1

  INTEGER FUNCTION intf1a (a, b)
    IMPLICIT NONE
    INTEGER :: a, b
    intf1a = 42
  END FUNCTION intf1a

  INTEGER FUNCTION intf2 (a, b)
    IMPLICIT NONE
    REAL :: a, b
    intf2 = 42.0
  END FUNCTION intf2

  LOGICAL FUNCTION intf3 ()
    IMPLICIT NONE
    intf3 = .TRUE.
  END FUNCTION intf3

  SUBROUTINE subr (x)
    IMPLICIT NONE
    INTEGER :: x
  END SUBROUTINE subr

END MODULE m

! { dg-final { cleanup-modules "m" } }

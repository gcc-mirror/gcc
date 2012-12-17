! { dg-do compile }

! Check for constraints restricting arguments of ELEMENTAL procedures.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

CONTAINS

  IMPURE ELEMENTAL SUBROUTINE foobar &
    (a, & ! { dg-error "must be scalar" }
     b, & ! { dg-error "POINTER attribute" }
     c, & ! { dg-error "ALLOCATABLE attribute" }
     d) ! { dg-error "must have its INTENT specified or have the VALUE attribute" }
    INTEGER, INTENT(IN) :: a(:)
    INTEGER, POINTER, INTENT(IN) :: b
    INTEGER, ALLOCATABLE, INTENT(IN) :: c
    INTEGER :: d
  END SUBROUTINE foobar

END PROGRAM main

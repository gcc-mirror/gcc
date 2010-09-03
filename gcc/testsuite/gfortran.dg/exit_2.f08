! { dg-do compile }
! { dg-options "-std=f2008" }

! PR fortran/44709
! Check that the resolving of loop names in parent namespaces introduced to
! handle intermediate BLOCK's does not go too far and other sanity checks.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  
  EXIT ! { dg-error "is not within a construct" }
  EXIT foobar ! { dg-error "is unknown" }
  EXIT main ! { dg-error "is not a construct name" }

  mainLoop: DO
    CALL test ()
  END DO mainLoop

  otherLoop: DO
    EXIT mainLoop ! { dg-error "is not within construct 'mainloop'" }
  END DO otherLoop

CONTAINS

  SUBROUTINE test ()
    EXIT mainLoop ! { dg-error "is unknown" }
  END SUBROUTINE test

END PROGRAM main

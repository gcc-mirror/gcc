! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/44602
! Check for F2008 rejection of non-loop EXIT.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  myname: IF (.TRUE.) THEN
    EXIT myname ! { dg-error "Fortran 2008" }
  END IF myname
END PROGRAM main

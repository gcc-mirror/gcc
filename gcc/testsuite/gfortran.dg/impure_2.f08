! { dg-do compile }
! { dg-options "-std=f2008" }

! PR fortran/45197
! Check for errors with IMPURE.

! Contributed by Daniel Kraft, d@domob.eu.

MODULE m
  IMPLICIT NONE

CONTAINS

  IMPURE PURE SUBROUTINE foobar () ! { dg-error "must not appear both" }

  PURE ELEMENTAL IMPURE FUNCTION xyz () ! { dg-error "must not appear both" }

  IMPURE ELEMENTAL SUBROUTINE mysub ()
  END SUBROUTINE mysub

  PURE SUBROUTINE purified ()
    CALL mysub () ! { dg-error "is not PURE" }
  END SUBROUTINE purified

END MODULE m

! { dg-final { cleanup-modules "m" } }

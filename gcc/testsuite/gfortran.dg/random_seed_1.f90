! { dg-do compile }

! Emit a diagnostic for too small PUT array at compile time
! See PR fortran/37159

! Possible improvement:
! Provide a separate testcase for systems that support REAL(16),
! to test the minimum size of 12 (instead of 8).

PROGRAM random_seed_1
  IMPLICIT NONE
  INTEGER :: small(7)
  CALL RANDOM_SEED(PUT=small)   ! { dg-error "is too small" }
END PROGRAM random_seed_1

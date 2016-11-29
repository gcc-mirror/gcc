! { dg-do compile }

! Emit a diagnostic for too small PUT array at compile time
! See PR fortran/37159

! Updated to check for arrays of unexpected size,
! this also works for -fdefault-integer-8.
!

PROGRAM random_seed_1
  IMPLICIT NONE

  INTEGER, PARAMETER :: nbytes = 128

  ! +1 due to the special 'p' value in xorshift1024*
  ! '+1' to avoid out-of-bounds warnings
  INTEGER, PARAMETER    :: n = nbytes / KIND(n) + 2
  INTEGER, DIMENSION(n) :: seed

  ! Get seed, array too small
  CALL RANDOM_SEED(GET=seed(1:(n-2)))  ! { dg-error "too small" }

  ! Get seed, array bigger than necessary
  CALL RANDOM_SEED(GET=seed(1:n))

  ! Get seed, proper size
  CALL RANDOM_SEED(GET=seed(1:(n-1)))

  ! Put too few bytes
  CALL RANDOM_SEED(PUT=seed(1:(n-2)))  ! { dg-error "too small" }

  ! Put too many bytes
  CALL RANDOM_SEED(PUT=seed(1:n))

  ! Put the right amount of bytes
  CALL RANDOM_SEED(PUT=seed(1:(n-1)))
END PROGRAM random_seed_1

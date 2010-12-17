! { dg-do compile }

! Emit a diagnostic for too small PUT array at compile time
! See PR fortran/37159

! Possible improvement:
! Provide a separate testcase for systems that support REAL(16),
! to test the minimum size of 12 (instead of 8).
!
! Updated to check for arrays of unexpected size,
! this also works for -fdefault-integer-8.
!

PROGRAM random_seed_1
  IMPLICIT NONE

  ! Find out what the's largest kind size
  INTEGER, PARAMETER :: k1 = kind (0.d0)
  INTEGER, PARAMETER :: &
    k2 = max (k1, selected_real_kind (precision (0._k1) + 1))
  INTEGER, PARAMETER :: &
    k3 = max (k2, selected_real_kind (precision (0._k2) + 1))
  INTEGER, PARAMETER :: &
    k4 = max (k3, selected_real_kind (precision (0._k3) + 1))

  INTEGER, PARAMETER :: nbytes = MERGE(48, 32, k4 == 16)

  ! '+1' to avoid out-of-bounds warnings
  INTEGER, PARAMETER    :: n = nbytes / KIND(n) + 1
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

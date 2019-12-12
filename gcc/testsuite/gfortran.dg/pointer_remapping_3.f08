! { dg-do compile }
! { dg-options "-std=f2008" }

! PR fortran/29785
! PR fortran/45016
! PR fortran/60091
! Check for pointer remapping compile-time errors.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, TARGET :: arr(12), basem(3, 4)
  INTEGER, POINTER :: vec(:), mat(:, :)

  ! Existence of reference elements.
  vec(:) => arr ! { dg-error "or list of 'lower-bound : upper-bound'" }
  vec(5:7:1)  => arr ! { dg-error "Stride must not be present" }
  mat(1:,2:5) => arr ! { dg-error "Rank remapping requires a list of " }
  mat(1:3,4:) => arr ! { dg-error "Rank remapping requires a list of " }
  mat(2, 6)   => arr ! { dg-error "Expected bounds specification" }

  mat(1:,3:)  => arr ! { dg-error "Rank remapping requires a list of " }

  ! Invalid remapping target; for non-rank one we already check the F2008
  ! error elsewhere.  Here, test that not-contiguous target is disallowed
  ! with rank > 1.
  mat(1:2, 1:3) => arr(1:12:2) ! This is ok, rank one target.
  vec(1:8) => basem(1:3:2, :) ! { dg-error "rank 1 or simply contiguous" }

  ! Target is smaller than pointer.
  vec(1:20) => arr ! { dg-error "smaller than size of the pointer" }
  vec(1:10) => arr(1:12:2) ! { dg-error "smaller than size of the pointer" }
  vec(1:20) => basem(:, :) ! { dg-error "smaller than size of the pointer" }
  mat(1:5, 1:5) => arr ! { dg-error "smaller than size of the pointer" }
END PROGRAM main

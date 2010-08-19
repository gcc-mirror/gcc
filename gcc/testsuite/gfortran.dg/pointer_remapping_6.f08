! { dg-do run }
! { dg-options "-std=f2008 -fcheck=bounds" }
! { dg-shouldfail "Bounds check" }

! PR fortran/29785
! Check that -fcheck=bounds catches too small target at runtime for
! pointer rank remapping.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, POINTER :: ptr(:, :)
  INTEGER :: n

  n = 10
  BLOCK
    INTEGER, TARGET :: arr(2*n)

    ! These are ok.
    ptr(1:5, 1:2) => arr
    ptr(1:5, 1:2) => arr(::2)
    ptr(-5:-1, 11:14) => arr

    ! This is not.
    ptr(1:3, 1:5) => arr(::2)
  END BLOCK
END PROGRAM main
! { dg-output "At line 26 of .*\nFortran runtime error: Target of rank remapping is too small \\(10 < 15\\)" }

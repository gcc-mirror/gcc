! { dg-do run }
!
! PR fortran/36132
!
! Before invalid memory was accessed because an absent, optional
! argument was packed before passing it as absent actual.
! Getting it to crash is difficult, but valgrind shows the problem.
!
MODULE M1
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
CONTAINS
  SUBROUTINE S1(a)
         REAL(dp), DIMENSION(45), INTENT(OUT), &
      OPTIONAL                               :: a
      if (present(a)) STOP 1
  END SUBROUTINE S1
  SUBROUTINE S2(a)
          REAL(dp), DIMENSION(:, :), INTENT(OUT), &
      OPTIONAL                               :: a
      CALL S1(a)
  END SUBROUTINE
END MODULE M1

USE M1
CALL S2()
END

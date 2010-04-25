! { dg-do run }
! { dg-options "-fdump-tree-original" }
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
      if (present(a)) call abort()
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

! { dg-final { scan-tree-dump-times "a != 0B \\? \\\(.*\\\) _gfortran_internal_pack" 1 "original" } }
! { dg-final { scan-tree-dump-times "if \\(a != 0B &&" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
! { dg-final { cleanup-modules "m1" } }

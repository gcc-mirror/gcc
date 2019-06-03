! { dg-do run }
! { dg-options "-O -fdump-tree-original" }
! Test handling of the optional argument.

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
! { dg-final { scan-tree-dump-times "arg_ptr" 5 "original" } }
! { dg-final { scan-tree-dump-not "_gfortran_internal_unpack" "original" } }

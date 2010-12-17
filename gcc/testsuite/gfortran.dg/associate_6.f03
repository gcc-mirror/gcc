! { dg-do compile }
! { dg-options "-std=f2003 -fdump-tree-original" }

! PR fortran/38936
! Check that array expression association (with correct bounds) works for
! complicated expressions.

! Contributed by Daniel Kraft, d@domob.eu.

MODULE m
  IMPLICIT NONE

CONTAINS

  PURE FUNCTION func (n)
    INTEGER, INTENT(IN) :: n
    INTEGER :: func(2 : n+1)

    INTEGER :: i

    func = (/ (i, i = 1, n) /)
  END FUNCTION func

END MODULE m

PROGRAM main
  USE :: m
  IMPLICIT NONE

  ASSOCIATE (arr => func (4))
    ! func should only be called once here, not again for the bounds!

    IF (LBOUND (arr, 1) /= 1 .OR. UBOUND (arr, 1) /= 4) CALL abort ()
    IF (arr(1) /= 1 .OR. arr(4) /= 4) CALL abort ()
  END ASSOCIATE
END PROGRAM main
! { dg-final { cleanup-modules "m" } }
! { dg-final { scan-tree-dump-times "func" 2 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

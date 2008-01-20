! { dg-do compile }
!
! PR fortran/34784, in which the intrinsic expression would be
! given the implicit type.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
MODULE m
  implicit character(s)
  INTEGER :: I(1) = (/ (SELECTED_INT_KIND(J),J=1,1) /)
END MODULE m

MODULE s_TESTS
  IMPLICIT CHARACTER (P)
CONTAINS
  subroutine simple (u,j1)
    optional ::  j1
    if (present (j1)) stop
  end subroutine
END MODULE s_TESTS

! { dg-final { cleanup-modules "m s_TESTS" } }

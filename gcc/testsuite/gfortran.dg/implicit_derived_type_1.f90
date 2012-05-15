! { dg-do compile }

! PR fortran/36746
! Check that parsing of component references for symbols with IMPLICIT
! derived-type works.

! Reduced test from the PR.
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
  type t
    integer :: i
  end type t
contains
  subroutine s(x)
    implicit type(t)(x)
    dimension x(:)
    print *, x(1)%i
  end subroutine s
end module m

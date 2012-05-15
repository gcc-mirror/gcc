! { dg-do compile }
! This tests that the fix for PR25391 also fixes PR20244. If
! the USE mod1 in subroutine foo were deleted, the code would
! compile fine.  With the USE statement, the compiler would
! make new TYPEs for T1 and T2 and bomb out in fold-convert.
! This is a slightly more elaborate test than
! used_dummy_types_1.f90 and came from the PR.
!
! Contributed by Jakub Jelinek  <jakubcc.gnu.org>
module mod1
  type t1
    real :: f1
  end type t1
  type t2
    type(t1), pointer :: f2(:)
    real, pointer :: f3(:,:)
  end type t2
end module mod1

module mod2
  use mod1
  type(t1), pointer, save :: v(:)
contains
  subroutine foo (x)
    use mod1
    implicit none
    type(t2) :: x
    integer :: d
    d = size (x%f3, 2)
    v = x%f2(:)
  end subroutine foo
end module mod2

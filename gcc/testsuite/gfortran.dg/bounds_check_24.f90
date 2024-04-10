! { dg-do compile }
! { dg-additional-options "-fcheck=bounds -fdump-tree-original" }
!
! PR fortran/113471 - wrong array bounds check

program pr113471
  implicit none
  type t
     integer, dimension(2) :: c1 = 0
  end type t
  type(t) :: cc(7), bb(7)
  integer :: kk = 1

  ! no bounds check (can be determined at compile time):
  call foo (cc(7)% c1)

  ! bounds check involving kk, but no "outside of expected range"
  call foo (bb(kk)% c1)

contains
  subroutine foo (c)
    integer, intent(in) :: c(:)
  end
end

! { dg-final { scan-tree-dump-times "below lower bound" 2 "original" } }
! { dg-final { scan-tree-dump-times "above upper bound" 2 "original" } }
! { dg-final { scan-tree-dump-not "outside of expected range" "original" } }

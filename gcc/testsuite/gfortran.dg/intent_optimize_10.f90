! { dg-do run }
! { dg-additional-options "-fno-inline -fno-ipa-modref -fdump-tree-optimized -fdump-tree-original" }
!
! PR fortran/41453
! Check that the INTENT(OUT) attribute causes in the case of non-polymorphic derived type arguments:
!  - one clobber to be emitted in the caller before calls to FOO in the *.original dump,
!  - no clobber to be emitted in the caller before calls to BAR in the *.original dump,
!  - the initialization constants to be optimized away in the *.optimized dump.

module x
  implicit none
  type :: t
    integer :: c
  end type t
  type, extends(t) :: u
    integer :: d
  end type u
contains
  subroutine foo(a)
    type(t), intent(out) :: a
    a = t(42)
  end subroutine foo
  subroutine bar(b)
    class(t), intent(out) :: b
    b%c = 24
  end subroutine bar
end module x

program main
  use x
  implicit none
  type(t) :: tc
  type(u) :: uc, ud
  class(t), allocatable :: te, tf

  tc = t(123456789)
  call foo(tc)
  if (tc%c /= 42) stop 1

  uc = u(987654321, 0)
  call foo(uc%t)
  if (uc%c /= 42) stop 2
  if (uc%d /= 0) stop 3

  ud = u(11223344, 0)
  call bar(ud)
  if (ud%c /= 24) stop 4

  te = t(55667788)
  call foo(te)
  if (te%c /= 42) stop 5

  tf = t(99887766)
  call bar(tf)
  if (tf%c /= 24) stop 6

end program main

! We don't support class descriptors, neither derived type components, so there is a clobber for tc only;
! no clobber for uc, ud, te, tf.
! { dg-final { scan-tree-dump-times "CLOBBER" 1 "original" } }
! { dg-final { scan-tree-dump "tc = {CLOBBER};" "original" } }

! There is a clobber for tc, so we should manage to optimize away the associated initialization constant (but not other
! initialization constants).
! { dg-final { scan-tree-dump-not "123456789" "optimized" { target __OPTIMIZE__ } } }

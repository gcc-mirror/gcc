! { dg-do run }
! { dg-additional-options "-fno-inline -fno-ipa-modref -fdump-tree-optimized -fdump-tree-original" }
!
! PR fortran/41453
! Check that the INTENT(OUT) attribute causes one clobber to be emitted in
! the caller before each call to FOO in the *.original dump, and the
! initialization constants to be optimized away in the *.optimized dump,
! in the case of SAVE variables.

module x
implicit none
contains
  subroutine foo(a)
    integer, intent(out) :: a
    a = 42
  end subroutine foo
end module x

program main
  use x
  implicit none
  integer :: c = 0

  ! implicit SAVE attribute
  c = 123456789
  call foo(c)
  if (c /= 42) stop 1

  ! explicit SAVE attribute
  call check_save_explicit

contains
  subroutine check_save_explicit
    integer, save :: d
    d = 987654321
    call foo(d)
    if (d /= 42) stop 2
  end subroutine check_save_explicit
end program main

! { dg-final { scan-tree-dump-times "CLOBBER" 2 "original" } }
! { dg-final { scan-tree-dump "c = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump "d = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump-not "123456789" "optimized" { target __OPTIMIZE__ } } }
! { dg-final { scan-tree-dump-not "987654321" "optimized" { target __OPTIMIZE__ } } }

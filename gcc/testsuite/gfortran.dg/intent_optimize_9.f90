! { dg-do run }
! { dg-additional-options "-fno-inline -fno-ipa-modref -fdump-tree-optimized -fdump-tree-original" }
!
! PR fortran/41453
! Check that the INTENT(OUT) attribute causes one clobber to be emitted in
! the caller before each call to FOO in the *.original dump, and the
! initialization constants to be optimized away in the *.optimized dump,
! in the case of scalar allocatables and pointers.

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
  integer, allocatable :: ca
  integer, target :: ct
  integer, pointer :: cp

  allocate(ca)
  ca = 123456789
  call foo(ca)
  if (ca /= 42) stop 1
  deallocate(ca)

  ct = 987654321
  cp => ct
  call foo(cp)
  if (ct /= 42) stop 2
end program main

! { dg-final { scan-tree-dump-times "CLOBBER" 2 "original" } }
! { dg-final { scan-tree-dump "\\*ca = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump "\\*cp = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump-not "123456789" "optimized" { target __OPTIMIZE__ } } }
! { dg-final { scan-tree-dump-not "987654321" "optimized" { target __OPTIMIZE__ } } }

! { dg-do run }
! { dg-additional-options "-fno-inline -fno-ipa-modref -fdump-tree-optimized -fdump-tree-original" }
!
! PR fortran/41453
! Check that the INTENT(OUT) attribute causes one clobber to be emitted in
! the caller before each call to FOO in the *.original dump, and the
! initialization constants to be optimized away in the *.optimized dump,
! in the case of associate variables.

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
  integer :: c1, c2

  c1 = 123456789
  associate (d1 => c1)
    call foo(d1)
    if (d1 /= 42) stop 1
  end associate
  if (c1 /= 42) stop 2

  c2 = 0
  associate (d2 => c2)
    d2 = 987654321
    call foo(d2)
    if (d2 /= 42) stop 3
  end associate
  if (c2 /= 42) stop 4

end program main

! { dg-final { scan-tree-dump-times "CLOBBER" 2 "original" } }
! { dg-final { scan-tree-dump "d1 = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump "\\*d2 = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump-not "123456789" "optimized" { target __OPTIMIZE__ } } }
! { dg-final { scan-tree-dump-not "987654321" "optimized" { target __OPTIMIZE__ } } }

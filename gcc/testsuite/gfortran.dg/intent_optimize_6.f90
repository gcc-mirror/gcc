! { dg-do run }
! { dg-additional-options "-fno-inline -fno-ipa-modref -fdump-tree-optimized -fdump-tree-original" }
!
! PR fortran/41453
! Check that the INTENT(OUT) attribute causes one clobber to be emitted in
! the caller before each call to FOO in the *.original dump, and the
! initialization constant to be optimized away in the *.optimized dump,
! in the case of an argument passed by reference to the caller.

module x
implicit none
contains
  subroutine foo(a)
    integer(kind=4), intent(out) :: a
    a = 42
  end subroutine foo
  subroutine bar(b)
    integer(kind=4) :: b
    b = 123456789
    call foo(b)
  end subroutine bar
end module x

program main
  use x
  implicit none
  integer(kind=4) :: c
  call bar(c)
  if (c /= 42) stop 1
end program main

! { dg-final { scan-tree-dump-times "CLOBBER" 1 "original" } }
! { dg-final { scan-tree-dump "\\*\\\(integer\\\(kind=4\\\) \\*\\\) b = {CLOBBER};" "original" } }
! { dg-final { scan-tree-dump-not "123456789" "optimized" { target __OPTIMIZE__ } } }

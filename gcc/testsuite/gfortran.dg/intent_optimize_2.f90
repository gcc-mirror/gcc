! { dg-do compile }
! { dg-options "-O -fno-inline -fdump-tree-optimized -fdump-tree-original" }
! PR fortran/41453
! Check that there is one clobber in the *.original tree, plus that
! the constant 123456789 has been removed due to the INTENT(OUT).

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
  integer :: a
  a = 123456789
  call foo(a)
  print *,a
end program main

! { dg-final { scan-tree-dump-times "123456789" 0 "optimized" } }
! { dg-final { scan-tree-dump-times "CLOBBER" 1 "original" } }

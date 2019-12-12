! { dg-do compile }
! { dg-options "-O0 -fdump-tree-original" }
!
! PR fortran/36909
!
! Check that no unneeded internal_unpack is
! called (INTENT(IN)!).
!
program test
  implicit none
  integer :: a(3,3)
  call foo(a(1,:))
contains
  subroutine foo(x)
    integer,intent(in) :: x(3)
  end subroutine foo
end program test

! { dg-final { scan-tree-dump-times "_gfortran_internal_pack" 1 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_internal_unpack" 0 "original" } }

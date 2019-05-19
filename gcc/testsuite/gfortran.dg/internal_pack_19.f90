! { dg-do compile }
! { dg-options "-Os -fdump-tree-original" }
! Check that internal_pack is called with -Os.
module x
  implicit none
contains
  subroutine bar(a, n)
    integer, intent(in) :: n
    integer, intent(in), dimension(n) :: a
    print *,a
  end subroutine bar
end module x

program main
  use x
  implicit none
  integer, parameter :: n = 10
  integer, dimension(n) :: a
  integer :: i
  a = [(i,i=1,n)]
  call bar(a(n:1:-1),n)
end program main
! { dg-final { scan-tree-dump-times "_gfortran_internal_pack" 1 "original" } }

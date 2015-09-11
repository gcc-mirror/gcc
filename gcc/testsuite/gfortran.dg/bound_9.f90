! { dg-do  run }
! { dg-options "-fdump-tree-original" }
! Check for different combinations of lbound for dummy arrays,
! stressing empty arrays.  The assignments with "one =" should
! be simplified at compile time.
module tst
  implicit none
contains
  subroutine foo (a, b, one, m)
    integer, dimension(:), intent(in) :: a
    integer, dimension (-2:), intent(in) :: b
    integer, intent(out) :: one, m
    one = lbound(a,1)
    m = lbound(b,1)
  end subroutine foo

  subroutine bar (a, b, n, m)
    integer, dimension(:), allocatable, intent(inout) :: a
    integer, dimension(:), pointer, intent(inout) :: b
    integer, intent(out) :: n, m
    n = lbound(a,1)
    m = lbound(b,1)
  end subroutine bar

  subroutine baz (a, n, m, s)
    integer, intent(in) :: n,m
    integer, intent(out) :: s
    integer, dimension(n:m) :: a
    s = lbound(a,1)
  end subroutine baz

  subroutine qux (a, s, one)
    integer, intent(in) :: s
    integer, dimension(s) :: a
    integer, intent(out) :: one
    one = lbound(a,1)
  end subroutine qux
end module tst

program main
  use tst
  implicit none
  integer, dimension(3), target :: a, b
  integer, dimension(0) :: empty
  integer, dimension(:), allocatable :: x
  integer, dimension(:), pointer :: y
  integer :: n,m
  

  call foo(a,b,n,m)
  if (n .ne. 1 .or. m .ne. -2) call abort
  call foo(a(2:0), empty, n, m)
  if (n .ne. 1 .or. m .ne. 1) call abort
  call foo(empty, a(2:0), n, m)
  if (n .ne. 1 .or. m .ne. 1) call abort
  allocate (x(0))
  y => a(3:2)
  call bar (x, y, n, m)
  if (n .ne. 1 .or. m .ne. 1) call abort

  call baz(a,3,2,n)
  if (n .ne. 1) call abort

  call baz(a,2,3,n)
  if (n .ne. 2) call abort

  call qux(a, -3, n)
  if (n .ne. 1) call abort
end program main
! { dg-final { scan-tree-dump-times "\\*one = 1" 2 "original" } }

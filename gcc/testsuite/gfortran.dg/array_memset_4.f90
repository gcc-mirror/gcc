! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR 40598 - optimizing zero assignments via memset for
! array slices.

program memain
  integer, dimension(:,:), allocatable :: a
  integer, dimension(3,3) :: b
  integer, dimension(4,3) :: c
  allocate (a(3,3))
  a = 1
  a(:,2) = 0
  if (any (a /= reshape([1,1,1,0,0,0,1,1,1], shape(a)))) stop 1
  b = 2
  b(:2,3) = 0
  if (any (b /= reshape([2,2,2,2,2,2,0,0,2], shape(b)))) stop 2
  c = 3
  call foo(c,4,3,2)
  if (any(c /= reshape([3,3,3,3,3,0,0,0,3,3,3,3], shape(c)))) stop 3
contains
  subroutine foo(x,n,m,k)
    integer, intent(in) :: n, m, k
    integer, dimension(n,m), intent(inout) :: x
    x(k:,k) = 0
  end subroutine foo
end program memain
! { dg-final { scan-tree-dump-times "__builtin_memset" 3 "original" } }

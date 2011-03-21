! { dg-do run }
!
! PR fortran/47775
!
! Contributed by Fran Martinez Fadrique
!
! Before, a temporary was missing for generic procedured (cf. test())
! as the allocatable attribute was ignored for the check whether a
! temporary is required
!
module m
type t
contains
  procedure, NOPASS :: foo => foo
  generic :: gen => foo
end type t
contains
  function foo(i)
    integer, allocatable :: foo(:)
    integer :: i
    allocate(foo(2))
    foo(1) = i
    foo(2) = i + 10
  end function foo
end module m

use m
type(t) :: x
integer, pointer :: ptr1, ptr2
integer, target              :: bar1(2)
integer, target, allocatable :: bar2(:)

allocate(bar2(2))
ptr1 => bar1(2)
ptr2 => bar2(2)

bar1 = x%gen(1)
if (ptr1 /= 11) call abort()
bar1 = x%foo(2)
if (ptr1 /= 12) call abort()
bar2 = x%gen(3)
if (ptr2 /= 13) call abort()
bar2 = x%foo(4)
if (ptr2 /= 14) call abort()
bar2(:) = x%gen(5)
if (ptr2 /= 15) call abort()
bar2(:) = x%foo(6)
if (ptr2 /= 16) call abort()

call test()
end

subroutine test
interface gen
  procedure foo
end interface gen

integer, target :: bar(2)
integer, pointer :: ptr
bar = [1,2]
ptr => bar(2)
if (ptr /= 2) call abort()
bar = gen()
if (ptr /= 77) call abort()
contains
  function foo()
    integer, allocatable :: foo(:)
    allocate(foo(2))
    foo = [33, 77]
  end function foo
end subroutine test

! { dg-final { cleanup-modules "m" } }

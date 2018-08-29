! { dg-do run }
!
! PR fortran/63205
!
! Check that passing a CLASS function result to a derived TYPE works
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!

program test
  implicit none
  type t
    integer :: ii
  end type t
  type, extends(t) :: u
    real :: rr
  end type u
  type, extends(t) :: v
    real, allocatable :: rr(:)
  end type v
  type, extends(v) :: w
    real, allocatable :: rrr(:)
  end type w

  type(t) :: x, y(3)
  type(v) :: a, b(3)

  x = func1() ! scalar to scalar - no alloc comps
  if (x%ii .ne. 77) STOP 1

  y = func2() ! array to array - no alloc comps
  if (any (y%ii .ne. [1,2,3])) STOP 2

  y = func1() ! scalar to array - no alloc comps
  if (any (y%ii .ne. 77)) STOP 3

  x = func3() ! scalar daughter type to scalar - no alloc comps
  if (x%ii .ne. 99) STOP 4

  y = func4() ! array daughter type to array - no alloc comps
  if (any (y%ii .ne. [3,4,5])) STOP 5

  y = func3() ! scalar daughter type to array - no alloc comps
  if (any (y%ii .ne. [99,99,99])) STOP 6

  a = func5() ! scalar to scalar - alloc comps in parent type
  if (any (a%rr .ne. [10.0,20.0])) STOP 7

  b = func6() ! array to array - alloc comps in parent type
  if (any (b(3)%rr .ne. [3.0,4.0])) STOP 8

  a = func7() ! scalar daughter type to scalar - alloc comps in parent type
  if (any (a%rr .ne. [10.0,20.0])) STOP 9

  b = func8() ! array daughter type to array - alloc comps in parent type
  if (any (b(3)%rr .ne. [3.0,4.0])) STOP 10

  b = func7() ! scalar daughter type to array - alloc comps in parent type
  if (any (b(2)%rr .ne. [10.0,20.0])) STOP 11

! This is an extension of class_to_type_2.f90's test using a daughter type
! instead of the declared type.
  if (subpr2_array (g ()) .ne. 99 ) STOP 12
contains

  function func1() result(res)
    class(t), allocatable :: res
    allocate (res, source = t(77))
  end function func1

  function func2() result(res)
    class(t), allocatable :: res(:)
    allocate (res(3), source = [u(1,1.0),u(2,2.0),u(3,3.0)])
  end function func2

  function func3() result(res)
    class(t), allocatable :: res
    allocate (res, source = v(99,[99.0,99.0,99.0]))
  end function func3

  function func4() result(res)
    class(t), allocatable :: res(:)
    allocate (res(3), source = [v(3,[1.0,2.0]),v(4,[2.0,3.0]),v(5,[3.0,4.0])])
  end function func4

  function func5() result(res)
    class(v), allocatable :: res
    allocate (res, source = v(3,[10.0,20.0]))
  end function func5

  function func6() result(res)
    class(v), allocatable :: res(:)
    allocate (res(3), source = [v(3,[1.0,2.0]),v(4,[2.0,3.0]),v(5,[3.0,4.0])])
  end function func6

  function func7() result(res)
    class(v), allocatable :: res
    allocate (res, source = w(3,[10.0,20.0],[100,200]))
  end function func7

  function func8() result(res)
    class(v), allocatable :: res(:)
    allocate (res(3), source = [w(3,[1.0,2.0],[0.0]),w(4,[2.0,3.0],[0.0]),w(5,[3.0,4.0],[0.0])])
  end function func8


  integer function subpr2_array (x)
    type(t) :: x(:)
    if (any(x(:)%ii /= 55)) STOP 13
    subpr2_array = 99
  end function

  function g () result(res)
    integer i
    class(t), allocatable :: res(:)
    allocate (res(3), source = [(v (1, [1.0,2.0]), i = 1, 3)])
    res(:)%ii = 55
  end function g
end program test

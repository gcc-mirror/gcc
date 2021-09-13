! { dg-do run }
!
! In the course of fixing PR83118, lots of issues came up with class array
! assignment, where temporaries are generated. This testcase checks that
! it all works correctly.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module m
  implicit none
  type :: t1
    integer :: i
  CONTAINS
  end type
  type, extends(t1) :: t2
    real :: r
  end type

  interface operator(+)
    module procedure add_t1
  end interface

contains
  function add_t1 (a, b) result (c)
    class(t1), intent(in) :: a(:), b(:)
    class(t1), allocatable :: c(:)
    allocate (c, source = a)
    c%i = a%i + b%i
    select type (c)
      type is (t2)
      select type (b)
        type is (t2)
          c%r = c%r + b%r
      end select
    end select
  end function add_t1

end module m

subroutine test_t1
  use m
  implicit none

  class(t1), dimension(:), allocatable :: x, y

  x = [t2(1,10.0),t2(2,20.0),t2(3,30.0)]
  if (.not.check_t1 (x, [1,2,3], 2, [10, 20, 30]) ) stop 1

  y = x
  x = realloc_t1 (y)
  if (.not.check_t1 (x, [3,2,1], 1) ) stop 2

  x = realloc_t1 (x)
  if (.not.check_t1 (x, [2,3,1], 1) ) stop 3

  x = x([3,1,2])
  if (.not.check_t1 (x, [1,2,3], 1) ) stop 4

  x = x(3:1:-1) + y
  if (.not.check_t1 (x, [4,4,4], 1) ) stop 5

  x = y + x(3:1:-1)
  if (.not.check_t1 (x, [5,6,7], 2) ) stop 6

! Now check that the dynamic type survives assignments.
  x = [t2(1,10.0),t2(2,20.0),t2(3,30.0)]
  y = x

  x = y(3:1:-1)
  if (.not.check_t1 (x, [3,2,1], 2, [30,20,10]) ) stop 7

  x = x(3:1:-1) + y
  if (.not.check_t1 (x, [2,4,6], 2, [20,40,60]) ) stop 8

  x = x(3:1:-1)
  if (.not.check_t1 (x, [6,4,2], 2, [60,40,20]) ) stop 9

  x = x([3,2,1])
  if (.not.check_t1 (x, [2,4,6], 2, [20,40,60]) ) stop 10

contains

  function realloc_t1 (arg) result (res)
    class(t1), dimension(:), allocatable :: arg
    class(t1), dimension(:), allocatable :: res
    select type (arg)
      type is (t2)
        allocate (res, source = [t1 (arg(3)%i), t1 (arg(2)%i), t1 (arg(1)%i)])
      type is (t1)
        allocate (res, source = [t1 (arg(2)%i), t1 (arg(1)%i), t1 (arg(3)%i)])
    end select
  end function realloc_t1

  logical function check_t1 (arg, array, t, array2)
    class(t1) :: arg(:)
    integer :: array (:), t
    integer, optional :: array2(:)
    check_t1 = .true.
    select type (arg)
    type is (t1)
      if (any (arg%i .ne. array)) check_t1 = .false.
      if (t .eq. 2) check_t1 = .false.
    type is (t2)
      if (any (arg%i .ne. array)) check_t1 = .false.
      if (t .eq. 1) check_t1 = .false.
      if (present (array2)) then
        if (any(int (arg%r) .ne. array2)) check_t1 = .false.
      end if
    class default
      check_t1 = .false.
    end select
  end function check_t1

end subroutine test_t1

subroutine test_star
  use m
  implicit none

  class(*), dimension(:), allocatable :: x, y

  x = [t2(1,10.0),t2(2,20.0),t2(3,30.0)]
  if (.not.check_star (x, [1,2,3], 2) ) stop 11

  y = x
  x = realloc_star (y)
  if (.not.check_star (x, [3,2,1], 1) ) stop 12

  x = realloc_star (x)
  if (.not.check_star (x, [2,3,1], 1) ) stop 13

  x = x([3,1,2])
  if (.not.check_star (x, [1,2,3], 1) ) stop 14

  x = x(3:1:-1)
  if (.not.check_star (x, [3,2,1], 1) ) stop 15

! Make sure that all is similarly well with type t2.
  x = [t2(1,10.0),t2(2,20.0),t2(3,30.0)]

  x = x([3,1,2])
  if (.not.check_star (x, [3,1,2], 2, [30,10,20]) ) stop 16

  x = x(3:1:-1)
  if (.not.check_star (x, [2,1,3], 2, [20,10,30]) ) stop 17

contains

  function realloc_star (arg) result (res)
    class(*), dimension(:), allocatable :: arg
    class(*), dimension(:), allocatable :: res
    select type (arg)
      type is (t2)
         allocate (res, source = [t1 (arg(3)%i), t1 (arg(2)%i), t1 (arg(1)%i)])
      type is (t1)
         allocate (res, source = [t1 (arg(2)%i), t1 (arg(1)%i), t1 (arg(3)%i)])
    end select
  end function realloc_star

  logical function check_star (arg, array, t, array2)
    class(*) :: arg(:)
    integer :: array (:), t
    integer, optional :: array2(:)
    check_star = .true.
    select type (arg)
      type is (t1)
        if (any (arg%i .ne. array)) check_star = .false.
        if (t .eq. 2) check_star = .false.
      type is (t2)
        if (any (arg%i .ne. array)) check_star = .false.
        if (t .eq. 1) check_star = .false.
        if (present (array2)) then
          if (any (int(arg%r) .ne. array2)) check_star = .false.
        endif
      class default
        check_star = .false.
    end select
  end function check_star

end subroutine test_star


  call test_t1
  call test_star
end

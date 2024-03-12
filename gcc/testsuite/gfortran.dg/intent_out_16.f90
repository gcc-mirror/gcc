! { dg-do run }
!
! PR fortran/92178
! Re-order argument deallocation

program p
  implicit none
  integer,   allocatable :: a(:)
  class(*),  allocatable :: c(:)
  type t
    integer, allocatable :: a(:)
  end type t
  type(t) :: b
  integer :: k = -999

  ! Test based on original PR
  a = [1]
  call assign (a, (max(a(1),0)))
  if (allocated (a)) stop 9
  if (k /= 1)        stop 10

  ! Additional variations based on suggestions by Tobias Burnus
  ! to check that argument expressions are evaluated early enough
  a = [1, 2]
  call foo (allocated (a), size (a), test (a), a, allocated (a))
  if (allocated (a)) stop 11

  a = [1, 2]
  k = 1
  call foo (allocated (a), size (a), test (k*a), a, allocated (a))
  if (allocated (a)) stop 12

  b% a = [1, 2]
  call foo (allocated (b% a), size (b% a), test (b% a), b% a, allocated (b% a))
  if (allocated (b% a)) stop 13

  c = [3, 4]
  call bar (allocated (c), size (c), test2 (c), c, &
            allocated (c), size (c), test2 (c)     )
  if (allocated (c)) stop 14

contains

  subroutine assign (a, i)
    integer, allocatable, intent(out) :: a(:) 
    integer,              value  :: i
    k = i
  end subroutine

  subroutine foo (alloc, sz, tst, x, alloc2)
    logical, value :: alloc, tst
    integer, value :: sz
    logical        :: alloc2
    integer, allocatable, intent(out) :: x(:)
    if (allocated (x)) stop 1
    if (.not. alloc)   stop 2
    if (sz /= 2)       stop 3
    if (.not. tst)     stop 4
    if (.not. alloc2)  stop 15
  end subroutine foo
  !
  logical function test (zz)
    integer :: zz(2)
    test = zz(2) == 2
  end function test
  !
  subroutine bar (alloc, sz, tst, x, alloc2, sz2, tst2)
    logical, value :: alloc, tst, alloc2, tst2
    integer, value :: sz, sz2
    class(*), allocatable, intent(out) :: x(:)
    if (allocated (x)) stop 5
    if (.not. alloc)   stop 6
    if (sz /= 2)       stop 7
    if (.not. tst)     stop 8
    if (.not. alloc2)  stop 16
    if (sz2 /= 2)      stop 17
    if (.not. tst2)    stop 18
  end subroutine bar
  !
  logical function test2 (zz)
    class(*), intent(in) :: zz(:)
    select type (zz)
    type is (integer)
       test2 = zz(2) == 4
    class default
       stop 99
    end select
  end function test2
end

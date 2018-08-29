! { dg-do compile }
! { dg-options -std=f2003 }
!
! Is a copy of ptr_func_assign_1.f08 with checks for F2008 standard.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
module fcn_bar
contains
  function bar (arg, idx) result (res)
    integer, pointer :: res
    integer, target :: arg(:)
    integer :: idx
    res => arg (idx)
    res = 99
  end function
end module

module fcn_mydt
  type mydt
    integer, allocatable, dimension (:) :: i
  contains
    procedure, pass :: create
    procedure, pass :: delete
    procedure, pass :: fill
    procedure, pass :: elem_fill
  end type
contains
  subroutine create (this, sz)
    class(mydt) :: this
    integer :: sz
    if (allocated (this%i)) deallocate (this%i)
    allocate (this%i(sz))
    this%i = 0
  end subroutine
  subroutine delete (this)
    class(mydt) :: this
    if (allocated (this%i)) deallocate (this%i)
  end subroutine
  function fill (this, idx) result (res)
    integer, pointer :: res(:)
    integer :: lb, ub
    class(mydt), target :: this
    integer :: idx
    lb = idx
    ub = lb + size(this%i) - 1
    res => this%i(lb:ub)
  end function
  function elem_fill (this, idx) result (res)
    integer, pointer :: res
    class(mydt), target :: this
    integer :: idx
    res => this%i(idx)
  end function
end module

  use fcn_bar
  use fcn_mydt
  integer, target :: a(3) = [1,2,3]
  integer, pointer :: b
  integer :: foobar, z, i, ifill(4) = [2, 7, 19, 61], ifill2(2) = [1,2]
  type(mydt) :: dt
  foobar (z) = z**2 ! { dg-warning "Obsolescent feature: Statement function" }
  if (any (a .ne. [1,2,3])) STOP 1

! Assignment to pointer result is after procedure call.
  foo (a) = 77 ! { dg-error "Pointer procedure assignment" }

! Assignment within procedure applies.
  b => foo (a)
  if (b .ne. 99) STOP 2

! Use of index for assignment.
  bar (a, 2) = 99 ! { dg-error "Pointer procedure assignment" }
  if (any (a .ne. [99,99,3])) STOP 3

! Make sure that statement function still works!
  if (foobar (10) .ne. 100) STOP 4

  bar (a, 3) = foobar (9)! { dg-error "Pointer procedure assignment" }
  if (any (a .ne. [99,99,81])) STOP 5

! Try typebound procedure
  call dt%create (6)
  dt%elem_fill (3) = 42 ! { dg-error "Pointer procedure assignment" }
  if (dt%i(3) .ne. 42) STOP 6
  dt%elem_fill (3) = 42 + dt%elem_fill (3)! { dg-error "Pointer procedure assignment" }
  if (dt%i(3) .ne. 84) STOP 7
  dt%elem_fill (3) = dt%elem_fill (3) - dt%elem_fill (3)! { dg-error "Pointer procedure assignment" }
  if (dt%i(3) .ne. 0) STOP 8
! Array is now reset
  dt%fill (3) = ifill ! { dg-error "Pointer procedure assignment" }
  dt%fill (1) = [2,1] ! { dg-error "Pointer procedure assignment" }
  if (any (dt%i .ne. [2,1,ifill])) STOP 9
  dt%fill (1) = footoo (size (dt%i, 1)) ! { dg-error "Pointer procedure assignment" }
  if (any (dt%i .ne. [6,5,4,3,2,1])) STOP 10
  dt%fill (3) = ifill + dt%fill (3) ! { dg-error "Pointer procedure assignment" }
  if (any (dt%i .ne. [6,5,6,10,21,62])) STOP 11
  call dt%delete

contains
  function foo (arg)
    integer, pointer :: foo
    integer, target :: arg(:)
    foo => arg (1)
    foo = 99
  end function
  function footoo (arg) result(res)
    integer :: arg
    integer :: res(arg)
    res = [(arg - i, i = 0, arg - 1)]
  end function
end

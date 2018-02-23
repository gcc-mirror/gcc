! { dg-do run }
!
! PR fortran/48820
!
! Handle type/class for assumed-rank arrays
!
! FIXME: Passing a CLASS to a CLASS has to be re-enabled.
implicit none
type t
  integer :: i
end type

class(T), allocatable :: ac(:,:)
type(T), allocatable :: at(:,:)
integer :: i

allocate(ac(2:3,2:4))
allocate(at(2:3,2:4))

i = 0
call foo(ac)
call foo(at)
call bar(ac)
call bar(at)
if (i /= 12) STOP 1

contains
  subroutine bar(x)
    type(t) :: x(..)
    if (lbound(x,1) /= 1 .or. lbound(x,2) /= 1) STOP 2
    if (size(x) /= 6) STOP 3
    if (size(x,1) /= 2 .or. size(x,2) /= 3) STOP 4
    if (ubound(x,1) /= 2 .or. ubound(x,2) /= 3) STOP 5
    i = i + 1
    call foo(x)
    call bar2(x)
  end subroutine
  subroutine bar2(x)
    type(t) :: x(..)
    if (lbound(x,1) /= 1 .or. lbound(x,2) /= 1) STOP 6
    if (size(x) /= 6) STOP 7
    if (size(x,1) /= 2 .or. size(x,2) /= 3) STOP 8
    if (ubound(x,1) /= 2 .or. ubound(x,2) /= 3) STOP 9
    i = i + 1
  end subroutine
  subroutine foo(x)
    class(t) :: x(..)
    if (lbound(x,1) /= 1 .or. lbound(x,2) /= 1) STOP 10
    if (size(x) /= 6) STOP 11
    if (size(x,1) /= 2 .or. size(x,2) /= 3) STOP 12
    if (ubound(x,1) /= 2 .or. ubound(x,2) /= 3) STOP 13
    i = i + 1
    call foo2(x)
!    call bar2(x) ! Passing a CLASS to a TYPE does not yet work
  end subroutine
  subroutine foo2(x)
    class(t) :: x(..)
    if (lbound(x,1) /= 1 .or. lbound(x,2) /= 1) STOP 14
    if (size(x) /= 6) STOP 15
    if (size(x,1) /= 2 .or. size(x,2) /= 3) STOP 16
    if (ubound(x,1) /= 2 .or. ubound(x,2) /= 3) STOP 17
    i = i + 1
  end subroutine
end 

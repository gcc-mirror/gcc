! { dg-do run }
! PR 51284
! A scalar dummy with the VALUE attribute is associated with an anonymous
! copy of the actual argument (F2023, 15.5.2.4), so the callee must not be
! able to reach the actual argument's data.  Three things went wrong:
!
!   - a non-polymorphic derived type with allocatable components was copied
!     shallowly, so the callee wrote through to the actual argument;
!   - a CLASS dummy fed a non-polymorphic actual argument was declared to
!     take the class container by value but was handed its address, which
!     gave the callee a garbage _data pointer;
!   - a CLASS dummy fed a polymorphic actual argument shared the actual
!     argument's data outright.

module m
  implicit none

  type :: base
     integer :: i = 0
     integer, allocatable :: a(:)
  end type base

  type, extends(base) :: child
     character(:), allocatable :: c
  end type child

contains

  subroutine take_derived (x)
    type(base), value :: x
    if (.not. allocated (x%a)) stop 1
    if (any (x%a /= [1, 2, 3])) stop 2
    x%i = -1
    x%a = -1
    deallocate (x%a)
    if (allocated (x%a)) stop 3
  end subroutine take_derived

  subroutine take_class (x)
    class(base), value :: x
    if (.not. allocated (x%a)) stop 4
    if (any (x%a /= [1, 2, 3])) stop 5
    x%i = -1
    x%a = -1
    select type (x)
    type is (child)
       if (x%c /= 'orig') stop 6
       x%c = 'CLOBBERED, and longer than the original'
    end select
  end subroutine take_class

end module m

program test
  use m
  implicit none

  type(base)  :: b
  type(child) :: c
  class(base), allocatable :: p

  ! Non-polymorphic dummy, non-polymorphic actual.
  b%i = 1
  b%a = [1, 2, 3]
  call take_derived (b)
  call check_base (b, 7)

  ! Polymorphic dummy, non-polymorphic actual of the declared type.
  call take_class (b)
  call check_base (b, 8)

  ! Polymorphic dummy, non-polymorphic actual of an extended type.
  c%i = 1
  c%a = [1, 2, 3]
  c%c = 'orig'
  call take_class (c)
  call check_child (c, 9)

  ! Polymorphic dummy, polymorphic actual of the declared type.
  allocate (p, source = b)
  call take_class (p)
  call check_class (p, 10)
  deallocate (p)

  ! Polymorphic dummy, polymorphic actual of an extended type.
  allocate (p, source = c)
  call take_class (p)
  call check_class (p, 11)
  select type (p)
  type is (child)
     if (p%c /= 'orig') stop 12
  class default
     stop 13
  end select
  deallocate (p)

contains

  subroutine check_base (x, code)
    type(base), intent(in) :: x
    integer, intent(in) :: code
    if (x%i /= 1) stop 20
    if (.not. allocated (x%a)) stop 21
    if (any (x%a /= [1, 2, 3])) stop 22
    if (code < 0) stop 23
  end subroutine check_base

  subroutine check_child (x, code)
    type(child), intent(in) :: x
    integer, intent(in) :: code
    if (x%i /= 1) stop 24
    if (.not. allocated (x%a)) stop 25
    if (any (x%a /= [1, 2, 3])) stop 26
    if (x%c /= 'orig') stop 27
    if (code < 0) stop 28
  end subroutine check_child

  subroutine check_class (x, code)
    class(base), intent(in) :: x
    integer, intent(in) :: code
    if (x%i /= 1) stop 29
    if (.not. allocated (x%a)) stop 30
    if (any (x%a /= [1, 2, 3])) stop 31
    if (code < 0) stop 32
  end subroutine check_class

end program test

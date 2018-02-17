! { dg-do run }
!
! Tests functionality of recursive allocatable derived types.
!
module m
  type :: stack
    integer :: value
    integer :: index
    type(stack), allocatable :: next
  end type stack
end module

  use m
! Here is how to add a new entry at the top of the stack:
  type (stack), allocatable :: top, temp, dum

  call poke (1)
  call poke (2)
  call poke (3)
  if (top%index .ne. 3) STOP 1
  call output (top)
  call pop
  if (top%index .ne. 2) STOP 2
  call output (top)
  deallocate (top)
contains
  subroutine output (arg)
    type(stack), target, allocatable :: arg
    type(stack), pointer :: ptr

    if (.not.allocated (arg)) then
      print *, "empty stack"
      return
    end if

    print *, "        idx           value"
    ptr => arg
    do while (associated (ptr))
      print *, ptr%index, "   ", ptr%value
      ptr => ptr%next
    end do
  end subroutine
  subroutine poke(arg)
    integer :: arg
    integer :: idx
    if (allocated (top)) then
      idx = top%index + 1
    else
      idx = 1
    end if
    allocate (temp)
    temp%value = arg
    temp%index = idx
    call move_alloc(top,temp%next)
    call move_alloc(temp,top)
  end subroutine
  subroutine pop
    call move_alloc(top%next,temp)
    call move_alloc(temp,top)
  end subroutine
end

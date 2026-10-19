! { dg-do run }
!
! PR 127941
!
! Deallocating a CLASS object whose type has a non-allocatable derived
! type array component with nested allocatable components freed an
! invalid pointer in the finalization wrapper.

module m
  type leaf
    real, allocatable :: value
  end type
  type item
    integer :: tag = 1
    type(leaf), allocatable :: children(:)
  end type
  type container
    type(item) :: entries(1)
  end type
  type, extends(container) :: ext
    type(item) :: more(2,3)
  end type
end module

program p
  use m
  class(container), allocatable :: x, y(:)

  allocate (x)
  deallocate (x)

  allocate (x)
  allocate (x%entries(1)%children(2))
  allocate (x%entries(1)%children(2)%value, source = 1.0)
  deallocate (x)

  allocate (ext :: x)
  select type (x)
  type is (ext)
    allocate (x%more(2,3)%children(1))
    allocate (x%more(2,3)%children(1)%value, source = 2.0)
  end select
  deallocate (x)

  allocate (y(3))
  allocate (y(2)%entries(1)%children(4))
  allocate (y(2)%entries(1)%children(3)%value, source = 3.0)
  deallocate (y)
end program

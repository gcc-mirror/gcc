! { dg-do compile }

! The finalization wrapper of a type with a derived type array component
! nested inside an array component of lower rank caused an ICE in
! gfc_get_descriptor_field.

module m
  type leaf
    real, allocatable :: value
  end type
  type item
    type(leaf), allocatable :: children(:)
  end type
  type mid
    integer :: k
    type(item) :: its(2,3)
  end type
  type container
    type(mid) :: mids(3)
  end type
  type deep
    integer :: k
    type(leaf) :: leaves(2,2,2)
  end type
  type, extends(container) :: ext
    type(deep) :: deeps(2,2)
  end type
contains
  subroutine s (x)
    class(container), allocatable :: x
    deallocate (x)
  end subroutine
end module

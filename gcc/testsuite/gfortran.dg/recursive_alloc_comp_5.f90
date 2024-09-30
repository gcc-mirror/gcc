!{ dg-do run }

! Check that PR106606 is fixed.

! Contributed by Ron Shepard  <shepard@tcg.anl.gov>

module bst_base_mod

  ! Binary Search Tree Module

  implicit none

  public

  type, abstract :: bst_base_node_type
    class(bst_base_node_type), allocatable :: left
    class(bst_base_node_type), allocatable :: right
  end type bst_base_node_type

  type, extends (bst_base_node_type) :: bst_base
    integer :: bst_base_value
  end type bst_base

end module bst_base_mod

  use bst_base_mod

  class (bst_base), allocatable :: root

  allocate (root, source = bst_base (NULL(), NULL(), 0))
  root%left = bst_base (NULL(), NULL(), 1)
  root%right = bst_base (NULL(), NULL(), 2)

  if (.not. allocated(root%left)) stop 1
  if (.not. allocated(root%right)) stop 2
end


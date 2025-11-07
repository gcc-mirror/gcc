! { dg-do run }
! Test self-assignment with recursive allocatable and finalizer
! This should preserve allocatable components after a = a and a = (a)

module self_assign_mod
  implicit none
  type :: node_t
     integer :: value = 0
     type(node_t), allocatable :: next
  contains
     final :: finalize_node
  end type node_t
contains
  subroutine finalize_node(self)
    type(node_t), intent(inout) :: self
  end subroutine finalize_node
end module self_assign_mod

program test_self_assign
  use self_assign_mod
  implicit none

  call test_simple_self_assign()
  call test_parenthesized_self_assign()
  call test_triple_parenthesized_self_assign()
  call test_array_bounds()

contains

  subroutine test_simple_self_assign()
    type(node_t) :: a

    a%value = 100
    allocate(a%next)
    a%next%value = 200

    ! Simple self-assignment should preserve all components
    a = a

    if (a%value /= 100) stop 1
    if (.not. allocated(a%next)) stop 2
    if (a%next%value /= 200) stop 3
  end subroutine test_simple_self_assign

  subroutine test_parenthesized_self_assign()
    type(node_t) :: a

    a%value = 100
    allocate(a%next)
    a%next%value = 200

    ! Parenthesized self-assignment should also preserve all components
    a = (a)

    if (a%value /= 100) stop 4
    if (.not. allocated(a%next)) stop 5
    if (a%next%value /= 200) stop 6
  end subroutine test_parenthesized_self_assign

  subroutine test_triple_parenthesized_self_assign()
    type(node_t) :: a

    a%value = 100
    allocate(a%next)
    a%next%value = 200

    ! Triple-nested parentheses should also work correctly
    a = (((a)))

    if (a%value /= 100) stop 7
    if (.not. allocated(a%next)) stop 8
    if (a%next%value /= 200) stop 9
  end subroutine test_triple_parenthesized_self_assign

  subroutine test_array_bounds()
    type(node_t), allocatable :: b(:), c(:)

    ! Test array bounds behavior with parentheses.
    ! Per F2023:10.2.1.3, lbound((b),1) = 1 even if lbound(b,1) = 5.
    ! However, for b = (b) where b is already allocated with the right shape,
    ! NO reallocation occurs, so bounds are preserved.
    ! For c = (b) where c is unallocated, c gets allocated with default bounds.
    allocate(b(5:5))
    b(5)%value = 500

    ! Self-assignment with parentheses: no reallocation (same shape), bounds preserved
    b = (b)
    if (.not. allocated(b)) stop 10
    if (lbound(b, 1) /= 5) stop 11  ! Bounds preserved (no realloc)
    if (ubound(b, 1) /= 5) stop 12
    if (b(5)%value /= 500) stop 13

    ! Assignment to unallocated array: gets default (1-based) bounds
    c = (b)
    if (.not. allocated(c)) stop 14
    if (lbound(c, 1) /= 1) stop 15  ! Default bounds (new allocation)
    if (ubound(c, 1) /= 1) stop 16
    if (c(1)%value /= 500) stop 17
  end subroutine test_array_bounds

end program test_self_assign

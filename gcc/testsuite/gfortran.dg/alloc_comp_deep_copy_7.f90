! { dg-do compile }
!
! PR fortran/121628
! Test that derived types with multiple recursive allocatable array
! components compile without ICE. This was broken by the initial deep-copy
! patch which caused infinite compile-time recursion due to seen_derived_types
! persisting across wrapper generation.
!
! The fix saves and restores seen_derived_types when generating element
! copy wrappers to prevent inheriting parent context state.
!

program alloc_comp_deep_copy_7
  implicit none

  type :: nested_t
     type(nested_t), allocatable :: children(:)
     type(nested_t), allocatable :: relatives(:)
  end type nested_t

  type(nested_t) :: a

end program alloc_comp_deep_copy_7

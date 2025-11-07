! { dg-do run }
! { dg-output " finalizing id\\s+0\\n finalizing id\\s+1\\n finalizer count =\\s+2\\n" }
! PR fortran/90519

module pr90519_finalizer_run_mod
  implicit none
  integer :: finalizer_count = 0
  type :: tree_t
     integer :: id = -1
     type(tree_t), allocatable :: child
  contains
     final :: finalize_tree
  end type tree_t
contains
  subroutine finalize_tree(self)
    type(tree_t), intent(inout) :: self
    finalizer_count = finalizer_count + 1
    print *, 'finalizing id', self%id
  end subroutine finalize_tree
end module pr90519_finalizer_run_mod

program test_finalizer
  use pr90519_finalizer_run_mod
  implicit none
  block
    type(tree_t) :: root
    root%id = 0
    allocate(root%child)
    root%child%id = 1
  end block
  print *, 'finalizer count =', finalizer_count
end program test_finalizer

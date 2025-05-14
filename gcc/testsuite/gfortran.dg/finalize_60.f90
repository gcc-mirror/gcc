!{ dg-do run }
!
! Check that the finalizer is called on unused variables too.
! Contributed by LXYAN  <z00823823@outlook.com>

module pr118730_mod
  implicit none
    
  logical :: finished = .FALSE.    

  type :: test_type
    integer::test 
  contains
    final :: finalize
  end type test_type

contains
  subroutine finalize(this)
    type(test_type), intent(inout) :: this
    finished = .TRUE.
  end subroutine finalize
end module pr118730_mod

program pr118730
  use :: pr118730_mod
  implicit none

  block
    type(test_type) :: test
  end block

  if (.NOT. finished) error stop 1
end program pr118730

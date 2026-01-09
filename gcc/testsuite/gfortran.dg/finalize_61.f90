! { dg-run }
!
! Test the fix for PR123483. The 'resourceManagerDestructor' was called once too often, with
! conditions shown in the comments below.
!
! Contributed by Andrew Benson  <abensonca@gmail.com>
!
module rm

  type :: resourceManager
     integer, pointer :: counter => null()
   contains
     final     :: resourceManagerDestructor
     procedure :: resourceManagerAssign
     generic   :: assignment(=) => resourceManagerAssign
  end type resourceManager

  interface resourceManager
      module procedure resourceManagerConstructor
  end interface resourceManager

  type :: base
   contains
     procedure :: baseAssignment
     generic :: assignment(=) => baseAssignment ! CONDITION: defined assignment triggered the bug.
  end type base
  
  type, extends(base) :: worker ! CONDITION: type being extension of another type triggered the bug...
     integer, allocatable, dimension(:) :: x ! ...together with this allocatable array.
     type(resourceManager) :: workspaceManager
  end type worker

  interface worker
     module procedure workConstructor
  end interface worker

contains

  function resourceManagerConstructor() result(self)
    type(resourceManager) :: self
    allocate(self%counter)
    self%counter=1
    return
  end function resourceManagerConstructor

  subroutine resourceManagerDestructor(self)
    implicit none
    type(resourceManager), intent(inout) :: self
    if (associated(self%counter)) then
       if (self%counter == 1) stop 1
       self%counter=self%counter-1
       if (self%counter == 0) deallocate(self%counter)
    end if
    return
  end subroutine resourceManagerDestructor
  
  subroutine resourceManagerAssign(to,from)
    implicit none
    class(resourceManager), intent(out) :: to
    class(resourceManager), intent(in) :: from
    if (associated(from%counter)) then
       to%counter  => from%counter
       to%counter=to%counter+1
    else
       to%counter  => null()
    end if
    return
  end subroutine resourceManagerAssign

  subroutine baseAssignment(self,from)
    class(base), intent(out) :: self
    class(base), intent(in) :: from
    select type (self)
    type is (worker)
       select type (from)
       type is (worker)
          self%workspaceManager=from%workspaceManager
       end select
    end select
  end subroutine baseAssignment
  
  function workConstructor() result(self)
    type(worker) :: self
    self%workspaceManager=resourceManager()
  end function workConstructor
  
end module rm

program duplicateFinalizationBug
  use rm
  type(worker) :: a
  
  a=worker()
  if (.not.associated (a%workspacemanager%counter) .or. &
      a%workspacemanager%counter .ne. 1) stop 2
end program duplicateFinalizationBug

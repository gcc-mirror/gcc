!{ dg-do run }

! Check gimplify with checking works. [PR86468]
! This rather complicated code is needed to produce two "different"
! types in the move_alloc.

! Contributed by Juergen Reuter  <juergen.reuter@desy.de>

module classes
  implicit none
  private
  public :: wrapped_coarray
  
  type :: wrapped_point
     integer, allocatable :: point(:)
   contains
     procedure :: add => wrapped_point_add
  end type wrapped_point
  
  type :: wrapped_coarray
     type(wrapped_point), allocatable :: caf(:)[:]
  end type wrapped_coarray
  
contains
  
  subroutine wrapped_point_add(self, to_add)
    class(wrapped_point), intent(inout) :: self
    integer, intent(in) :: to_add
    integer, allocatable :: point(:)
    integer :: points_number
    
    if (allocated(self%point)) then
       points_number = size(self%point, dim=1)
       allocate(point(1:points_number+1))
       point(1:points_number) = self%point
       point(points_number+1) = to_add
       call move_alloc(from=point, to=self%point)
    else
       allocate(self%point(1))
       self%point(1) = to_add
    end if
  end subroutine wrapped_point_add
end module classes

program test
  use classes
  implicit none
  
  type(wrapped_coarray) :: foo
  allocate(foo%caf(99)[*])
  call foo%caf(32)%add(this_image())
  call foo%caf(32)%add(this_image())
  if (any (foo%caf(32)%point /= [this_image(), this_image()])) stop 1
end program test


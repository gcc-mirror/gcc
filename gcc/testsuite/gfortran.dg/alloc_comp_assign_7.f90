! { dg-do run }
!
! Test the fix for PR37735, in which gfc gagged in the assignement to
! 'p'.  The array component 'r' caused an ICE.
!
! Contributed by Steven Winfield <saw44@cam.ac.uk>
!
module PrettyPix_module
  implicit none
  type Spline
     real, allocatable, dimension(:) ::y2
  end type Spline
  type Path
     type(Spline) :: r(3)
  end type Path
  type Scene
     type(path) :: look_at_path
  end type Scene
contains
  subroutine scene_set_look_at_path(this,p)
    type(scene), intent(inout) :: this
    type(path),  intent(in)    :: p
    this%look_at_path = p
  end subroutine scene_set_look_at_path
end module PrettyPix_module

  use PrettyPix_module
  implicit none
  integer :: i
  real :: x(3) = [1.0, 2.0, 3.0]
  type(scene) :: this
  type(path)  :: p
  p = path ([spline([x(1)]),spline([x(2)]),spline([x(3)])])
  call scene_set_look_at_path(this,p)
  do i = 1, 3
    if (this%look_at_path%r(i)%y2(1) .ne. x(i)) call abort
  end do
end

! { dg-final { cleanup-modules "prettypix_module" } }

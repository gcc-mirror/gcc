! { dg-do run }
! { dg-options "-fcheck=all" }
!
! PR 50225: [OOP] The allocation status for polymorphic allocatable function results is not set properly
!
! Contributed by Arjen Markus <arjen.markus895@gmail.com>

module points2d

  implicit none

  type point2d
      real :: x, y
  end type

contains

 subroutine print( point )
   class(point2d) :: point
   write(*,'(2f10.4)') point%x, point%y
 end subroutine

 subroutine random_vector( point )
   class(point2d) :: point
   call random_number( point%x )
   call random_number( point%y )
   point%x = 2.0 * (point%x - 0.5)
   point%y = 2.0 * (point%y - 0.5)
 end subroutine

 function add_vector( point, vector )
   class(point2d), intent(in)  :: point, vector
   class(point2d), allocatable :: add_vector
   allocate( add_vector )
   add_vector%x = point%x + vector%x
   add_vector%y = point%y + vector%y
 end function

end module points2d


program random_walk

  use points2d
  implicit none

  type(point2d), target   :: point_2d, vector_2d
  class(point2d), pointer :: point, vector
  integer :: i

  point  => point_2d
  vector => vector_2d

  do i=1,2
    call random_vector(point)
    call random_vector(vector)
    call print(add_vector(point, vector))
  end do

end program random_walk

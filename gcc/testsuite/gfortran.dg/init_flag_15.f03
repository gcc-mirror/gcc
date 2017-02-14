! { dg-do run }
! { dg-options "-finit-derived -finit-integer=1" }
!
! Make sure -finit-derived works on class variables.
! Based on class_result_1.f03
!

module points_2i

  implicit none

  type point2i
      integer :: x, y
  end type

contains

 subroutine print( point )
   class(point2i) :: point
   write(*,'(2i4)') point%x, point%y
 end subroutine

 subroutine set_vector( point, rx, ry )
   class(point2i) :: point
   integer :: rx, ry
   point%x = rx
   point%y = ry
 end subroutine

 function add_vector( point, vector )
   class(point2i), intent(in)  :: point, vector
   class(point2i), allocatable :: add_vector
   allocate( add_vector )
   add_vector%x = point%x + vector%x
   add_vector%y = point%y + vector%y
 end function

end module


program init_flag_15

  use points_2i
  implicit none

  type(point2i), target   :: point_2i, vector_2i
  class(point2i), pointer :: point, vector
  type(point2i) :: vsum
  integer :: i

  point  => point_2i ! = (1, 1) due to -finit-integer
  vector => vector_2i
  call set_vector(vector, 2, 2)
  vsum = add_vector(point, vector)

  call print(point)
  call print(vector)
  call print(vsum)

  if (vsum%x .ne. 3 .or. vsum%y .ne. 3) then
    call abort()
  endif

end program

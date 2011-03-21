! { dg-do run }
!
! PR 46838: [OOP] Initialization of polymorphic allocatable components
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

program bug28

  implicit none

  type indx_map
  end type

  type desc_type
    integer, allocatable         :: matrix_data
    class(indx_map), allocatable :: indxmap
  end type

  type(desc_type) :: desc_a
  call cdall(desc_a)

contains

  subroutine cdall(desc)
    type(desc_type), intent(out)  :: desc
    if (allocated(desc%indxmap)) call abort()
  end subroutine cdall

end program

! { dg-do run }
! PR43969 - class array implementation
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
  implicit none

  type indx_map
  end type

  type desc_type
    class(indx_map), allocatable :: indxmap(:)
  end type

  type(desc_type)  :: desc
  if (allocated(desc%indxmap)) STOP 1

end

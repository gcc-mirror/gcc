! { dg-do run }
!
! Test the fix for PR84523.
!
! Contributed by Harald Anlauf  <anlauf@gmx.de>
!
program gfcbug148
  implicit none
  integer, parameter :: nspots = 80
  type t_spot
    real, allocatable     :: vm(:,:,:)
  end type t_spot
  type t_rowcol
    integer               :: nh
    type(t_spot), pointer :: spots(:) => NULL ()
  end type t_rowcol
  type(t_rowcol)          :: col
  call construct (col, nspots)
  call destruct  (col)
  !========================================================================
contains
  !========================================================================
  subroutine construct (rc, nh)
    type(t_rowcol) ,intent(out) :: rc   ! row or column to set
    integer        ,intent(in)  :: nh   ! number of spots in a row
    rc%nh = nh
    allocate (rc%spots(nh))
  end subroutine construct
  !------------------------------------------------------------------------
  subroutine destruct (rc)
    type(t_rowcol) ,intent(inout) :: rc   ! row or column to free
    integer :: k
    if (associated (rc%spots)) then
      if (size(rc%spots) .ne. nspots) stop 1
      do k=1, size(rc% spots)
        if (allocated (rc%spots(k)%vm)) stop 2  ! Would segfault in runtime.
      end do
      deallocate (rc%spots)
    endif
    nullify (rc%spots)
  end subroutine destruct
end program gfcbug148

! { dg-do run }
! Tests the fix for PR34820, in which the nullification of the
! automatic array iregion occurred in the caller, rather than the
! callee.  Since 'nproc' was not available, an ICE ensued. During
! the bug fix, it was found that the scalar to array assignment
! of derived types with allocatable components did not work and
! the fix of this is tested too.
!
! Contributed by Toon Moene <toon@moene.indiv.nluug.nl>
!
module grid_io
  type grid_index_region
    integer, allocatable::lons(:)
  end type grid_index_region
contains
  subroutine read_grid_header()
    integer :: npiece = 1
    type(grid_index_region),allocatable :: iregion(:)
    allocate (iregion(npiece + 1))
    call read_iregion(npiece,iregion)
    if (size(iregion) .ne. npiece + 1) STOP 1
    if (.not.allocated (iregion(npiece)%lons)) STOP 2
    if (allocated (iregion(npiece+1)%lons)) STOP 3
    if (any (iregion(npiece)%lons .ne. [(i, i = 1, npiece)])) STOP 4
    deallocate (iregion)
  end subroutine read_grid_header

  subroutine read_iregion (nproc,iregion)
    integer,intent(in)::nproc
    type(grid_index_region), intent(OUT)::iregion(1:nproc)
    integer :: iarg(nproc)
    iarg = [(i, i = 1, nproc)]
    iregion = grid_index_region (iarg) !
  end subroutine read_iregion
end module grid_io

  use grid_io
  call read_grid_header
end

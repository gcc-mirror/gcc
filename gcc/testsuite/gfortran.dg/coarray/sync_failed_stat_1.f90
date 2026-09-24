! { dg-do run }
!
! SYNC ALL synchronizes the active images of a team that contains a failed
! image and has to report it through stat= (F2023 11.7.11), also when the
! image fails while the others already wait in the statement.

program sync_failed_stat_1
  use iso_fortran_env, only : stat_failed_image
  implicit none
  integer :: st

  if (num_images () < 2) stop
  if (this_image () == 1) then
    call spin ()
    fail image
  end if

  st = 0
  sync all (stat=st)
  if (st /= stat_failed_image) stop 1
contains
  subroutine spin ()
    integer :: c
    integer(kind=8) :: v
    v = 2
    do c = 1, 20000000
      v = mod (v * 2, 199679_8)
    end do
  end subroutine
end program sync_failed_stat_1

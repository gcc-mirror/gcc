! { dg-do run }
! { dg-shouldfail "an image is killed by a signal" }
!
! An image killed by a signal is a failed image (F2023 5.3.6).  The other
! images have to continue and report it, instead of blocking in the SYNC ALL
! below until the test times out.

program signal_sync_1
  use iso_fortran_env, only : stat_failed_image
  implicit none
  integer :: i, st

  sync all
  if (this_image () == num_images ()) call abort ()

  do i = 1, 100
    st = 0
    sync all (stat=st)
    if (st == stat_failed_image) exit
  end do
  if (num_images () > 1 .and. st /= stat_failed_image) error stop "not reported"
end program signal_sync_1

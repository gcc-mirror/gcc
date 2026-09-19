! { dg-do run }
!
! A collective subroutine has to report an image that terminated normally
! through stat= instead of blocking forever.

program collective_stopped_1
  use iso_fortran_env, only : stat_stopped_image
  implicit none
  integer :: st, val

  sync all
  if (num_images () > 1 .and. this_image () == num_images ()) stop

  st = 0
  sync all (stat=st)

  val = this_image ()
  st = 0
  call co_sum (val, stat=st)
  if (num_images () > 1) then
    if (st /= stat_stopped_image) error stop "co_sum missed the stopped image"
  else
    if (st /= 0) error stop "co_sum failed"
  end if

  st = 0
  call co_broadcast (val, 1, stat=st)
  if (num_images () > 1) then
    if (st /= stat_stopped_image) error stop "co_broadcast missed the stopped image"
  else
    if (st /= 0) error stop "co_broadcast failed"
  end if
end program collective_stopped_1

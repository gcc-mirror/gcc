! { dg-do run }
!
! An image terminating while the others are inside a collective subroutine
! has to be reported through stat=, not by terminating them.

program collective_stopped_2
  use iso_fortran_env, only : stat_stopped_image
  implicit none
  integer :: s[*]
  integer :: st, img

  if (num_images () == 1) stop
  img = max (num_images () / 2, 2)
  s = this_image ()
  if (this_image () == img) then
    call spin ()
    stop
  end if

  st = 0
  call co_sum (s, stat=st)
  if (st /= stat_stopped_image) stop 1
contains
  subroutine spin ()
    integer :: c
    integer(kind=8) :: v
    v = 2
    do c = 1, 20000000
      v = mod (v * 2, 1999619_8)
    end do
  end subroutine
end program collective_stopped_2

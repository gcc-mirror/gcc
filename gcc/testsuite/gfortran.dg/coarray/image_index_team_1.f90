! { dg-do run }
!
! PR fortran/126777
!
! IMAGE_INDEX ICEd in walk_coarray when given a TEAM or TEAM_NUMBER
! argument.  The allocatable coarray is not folded by
! gfc_simplify_image_index, so it reaches trans_image_index with
! -fcoarray=single as well.

program image_index_team_1
  use iso_fortran_env, only : team_type
  implicit none

  integer :: caf[*]
  integer, allocatable :: acaf(:)[:]
  type(team_type) :: t
  integer :: n, tn, ref

  n = num_images ()
  t = get_team ()
  tn = team_number ()

  ref = image_index (caf, [n])
  if (image_index (caf, [n], t) /= ref) stop 1
  if (image_index (caf, [n], get_team ()) /= ref) stop 2
  if (image_index (caf, [n], tn) /= ref) stop 3
  if (image_index (caf, [n], team_number ()) /= ref) stop 4

  allocate (acaf(2)[*])
  ref = image_index (acaf, [n])
  if (image_index (acaf, [n], t) /= ref) stop 5
  if (image_index (acaf, [n], get_team ()) /= ref) stop 6
  if (image_index (acaf, [n], tn) /= ref) stop 7
  if (image_index (acaf, [n], team_number ()) /= ref) stop 8
  deallocate (acaf)
end program image_index_team_1

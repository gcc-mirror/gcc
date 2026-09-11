! { dg-do run }
!
! PR 126781
!
! The TEAM and TEAM_NUMBER arguments of NUM_IMAGES and IMAGE_INDEX shared a
! single dummy named "team/team_number", so neither keyword could be used.

program num_images_team_1
  use iso_fortran_env, only : team_type
  implicit none

  integer :: caf[*]
  type(team_type) :: t
  integer :: n, tn

  n = num_images ()
  t = get_team ()
  tn = team_number ()

  if (num_images (t) /= n) stop 1
  if (num_images (tn) /= n) stop 2
  if (num_images (team = t) /= n) stop 3
  if (num_images (team_number = tn) /= n) stop 4
  if (num_images (team = get_team ()) /= n) stop 5
  if (num_images (team_number = team_number ()) /= n) stop 6

  if (image_index (caf, [n], team = t) /= n) stop 7
  if (image_index (caf, [n], team_number = tn) /= n) stop 8
  if (image_index (caf, [n], team = get_team ()) /= n) stop 9
  if (image_index (caf, [n], team_number = team_number ()) /= n) stop 10
  if (image_index (coarray = caf, sub = [n], team = t) /= n) stop 11
  if (image_index (sub = [n], coarray = caf, team_number = tn) /= n) stop 12
end program num_images_team_1

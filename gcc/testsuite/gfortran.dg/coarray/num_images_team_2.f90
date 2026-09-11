! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
!
! PR 126781
!
! TEAM and TEAM_NUMBER arguments of NUM_IMAGES and IMAGE_INDEX inside a
! CHANGE TEAM construct.

program num_images_team_2
  use iso_fortran_env, only : team_type, initial_team, parent_team
  implicit none

  integer :: caf[*]
  type(team_type) :: t
  integer :: n, m, tn

  n = num_images ()
  form team (2 - mod (this_image (), 2), t)

  change team (t)
    m = num_images ()
    tn = team_number ()

    if (num_images (team_number = tn) /= m) stop 1
    if (num_images (tn) /= m) stop 2
    if (num_images (team_number = -1) /= n) stop 3
    if (num_images (team = get_team (initial_team)) /= n) stop 4
    if (num_images (team = get_team (parent_team)) /= n) stop 5
    if (num_images (get_team ()) /= m) stop 6

    if (image_index (caf, [1], team_number = tn) /= 1) stop 7
    if (image_index (caf, [m], team_number = tn) /= m) stop 8
    if (image_index (caf, [m + 1], team_number = tn) /= 0) stop 9
    if (image_index (caf, [n], team = get_team (initial_team)) /= n) stop 10
    if (image_index (caf, [n + 1], team = get_team (parent_team)) /= 0) stop 11
    if (image_index (caf, [m], get_team ()) /= m) stop 12
  end team
end program num_images_team_2

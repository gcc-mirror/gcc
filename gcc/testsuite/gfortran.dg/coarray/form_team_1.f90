!{ dg-do run }

program main
  use, intrinsic :: iso_fortran_env, only: team_type
  implicit none
  type(team_type) :: team
  integer :: slice_size, team_no

  if (num_images() >= 3) then
    slice_size = num_images() / 3
    team_no = this_image() / slice_size + 1

    form team (team_no, team)

    sync all
  end if

end program

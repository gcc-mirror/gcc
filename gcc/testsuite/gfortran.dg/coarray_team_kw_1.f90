! { dg-do compile }
! { dg-options "-fcoarray=lib" }
!
! PR 126781
!
! TEAM and TEAM_NUMBER are separate arguments of NUM_IMAGES and IMAGE_INDEX:
! each keyword accepts only its own type, and at most one may be given.

program coarray_team_kw_1
  use iso_fortran_env, only : team_type
  implicit none

  integer :: caf[*], r, n
  type(team_type) :: t

  t = get_team ()
  n = 1

  r = num_images (team = n) ! { dg-error "shall be of type 'team_type'" }
  r = num_images (team_number = t) ! { dg-error "must be INTEGER" }
  r = num_images (team = t, team_number = n) ! { dg-error "are mutually exclusive" }

  r = image_index (caf, [1], team = n) ! { dg-error "shall be of type 'team_type'" }
  r = image_index (caf, [1], team_number = t) ! { dg-error "must be INTEGER" }
  r = image_index (caf, [1], team = t, team_number = n) ! { dg-error "are mutually exclusive" }
end program coarray_team_kw_1

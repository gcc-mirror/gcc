! { dg-do compile }
! { dg-options "-fcoarray=lib -fdefault-integer-8" }
! { dg-require-effective-target fortran_integer_16 }
! PR fortran/84784 - ICEs: verify_gimple failed with -fdefault-integer-8

  use iso_fortran_env, only : team_type, STAT_FAILED_IMAGE
  implicit none
  type(team_type) :: team
  integer         :: new_team
  new_team = mod(this_image(),2)+1
  form team (new_team,team)
    change team (team)
    if (team_number() /= new_team) STOP 1
  end team
  if (image_status (1) == STAT_FAILED_IMAGE) ERROR STOP "cannot recover"
  if (runtime_popcnt(0_16) /= 0) STOP 2
  if (runtime_poppar(1_16) /= 1) STOP 3
contains
  integer function runtime_popcnt (i)
    integer(kind=16), intent(in) :: i
    runtime_popcnt = popcnt(i)
  end function
  integer function runtime_poppar (i)
    integer(kind=16), intent(in) :: i
    runtime_poppar = poppar(i)
  end function
end

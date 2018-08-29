! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Tests if form team works
!
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team

  form team (mod(this_image(),2)+1,team)

end

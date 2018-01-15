! { dg-do run }
! { dg-options "-fcoarray=single" }
!
!
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team

  form team (this_image(),team)
end



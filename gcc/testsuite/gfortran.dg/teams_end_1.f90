! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Tests if end team reverts this_image value
!
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: team
  integer :: orig_i

  orig_i = this_image ()
  form team (orig_i + 1,team)
  change team (team)
  end team
  if ( orig_i .NE. this_image () ) call abort
end



! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/105526 - check TEAM arguments to coarray intrinsics

subroutine p
  use iso_fortran_env, only: team_type
  implicit none
  type(team_type) :: team
  type t
     integer :: i
  end type t
  type(t) :: z
  form team (0, team)
  form team (0, 0)      ! { dg-error "scalar expression of type TEAM_TYPE" }
  form team (0, [team]) ! { dg-error "scalar expression of type TEAM_TYPE" }
  form team ([0], team) ! { dg-error "scalar INTEGER" }
  form team (0., team)  ! { dg-error "scalar INTEGER" }
  change team (0)       ! { dg-error "scalar expression of type TEAM_TYPE" }
  end team
  sync team (0)         ! { dg-error "scalar expression of type TEAM_TYPE" }
end

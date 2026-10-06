! { dg-do run }
!
! The STAT= variable of the team statements must receive the value the
! library stores; without the fix it kept its old value at -O2.

program team_stat_1
  use iso_fortran_env, only : team_type
  implicit none
  type(team_type) :: t
  integer :: st

  st = 42
  form team (1, t, stat=st)
  if (st /= 0) error stop 1
  st = 42
  change team (t, stat=st)
    if (st /= 0) error stop 2
    st = 42
    sync team (t, stat=st)
    if (st /= 0) error stop 3
    st = 42
  end team (stat=st)
  if (st /= 0) error stop 4
end program team_stat_1

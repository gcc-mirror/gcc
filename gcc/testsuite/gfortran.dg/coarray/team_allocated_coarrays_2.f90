! { dg-do run }
!
! PR 127724 - ICE when a team block holds a single statement before
! END TEAM.

program prog
  use, intrinsic :: iso_fortran_env, only: team_type
  implicit none
  type(team_type) :: team
  integer, allocatable :: a[:], b(:)[:]
  integer :: team_id

  team_id = 1 + mod(this_image(), 2)
  form team (team_id, team)

  change team (team)
    if (team_id == 1) then
      allocate(a[*])
      deallocate(a)
    end if
  end team
  if (allocated (a)) stop 1

  change team (team)
    allocate(a[*], b(3)[*])
  end team
  if (allocated (a) .or. allocated (b)) stop 2

  change team (team)
    block
      allocate(a[*])
    end block
  end team
  if (allocated (a)) stop 3
end program

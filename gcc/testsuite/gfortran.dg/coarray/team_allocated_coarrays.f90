! { dg-do run }
!
! F2018(11.1.5.2): Test auto-deallocation of allocatable coarrays that are
! allocated within a team block.
!
program test_nested_teams
  use iso_fortran_env, only: team_type
  implicit none
  type(team_type) :: team1, team2
  integer, allocatable :: a[:]
  integer, allocatable :: b[:]
  integer, allocatable :: outer[:]
  integer, allocatable :: inner(:)[:] ! Rank and corank
  logical :: image1
  integer :: me

  type :: mytype
    integer, allocatable :: i[:]
  end type
  type(mytype) :: dt_a, dt_b, dt_outer, dt_inner

  image1 = this_image () == 1
  me = this_image ()

  ! Test 1: Simple allocation in single team block
  form team(1, team1)
  change team(team1)
    allocate(a[*])
    a = 1
    allocate(dt_a%i[*], source = me)
  end team
  if (image1 .and. allocated(a)) stop 1
  if (image1 .and. allocated(dt_a%i)) stop 2

  ! Test 2: Multiple allocations in single team block
  form team(1, team1)
  change team(team1)
    allocate(a[*], b[*])
    a = 1
    b = 2
    allocate(dt_a%i[*], dt_b%i[*], source = me)
  end team
  if (image1 .and. allocated(a)) stop 3
  if (image1 .and. allocated(b)) stop 4
  if (image1 .and. allocated(dt_a%i)) stop 5
  if (image1 .and. allocated(dt_b%i)) stop 6

  ! Test 3: Nested team blocks - allocation in outer team only
  form team(1, team1)
  change team(team1)
    allocate(outer[*])
    allocate(dt_outer%i[*], source = me)
    outer = 10

    ! Nested team with no allocations
    form team(1, team2)
    change team(team2)
    end team

    ! Make sure that auto-deallocation occurs in right context
    if (image1 .and. .not.allocated(outer)) stop 7
    if (image1 .and. .not.allocated(dt_outer%i)) stop 8

  end team
  if (image1 .and. allocated(outer)) stop 9
  if (image1 .and. allocated(dt_outer%i)) stop 10

  ! Test 4: Nested team blocks - allocation in inner team only
  form team(1, team1)
  change team(team1)

    form team(1, team2)
    change team(team2)
      allocate(inner(4)[*])
      inner = 20
      allocate(dt_inner%i[*])
    end team

    if (image1 .and. allocated(inner)) stop 11
    if (image1 .and. allocated(dt_inner%i)) stop 12
  end team

  ! Test 5: Nested team blocks - allocations in both levels
  form team(1, team1)
  change team(team1)
    allocate(outer[*])
    outer = 30
    allocate(dt_outer%i[*], source = me)

    form team(1, team2)
    change team(team2)
      allocate(inner(2)[*])
      inner = 40
      allocate(dt_inner%i[*], source = me)
    end team

    if (image1 .and. allocated(inner)) stop 13
    if (image1 .and. allocated(dt_inner%i)) stop 14

  end team
  if (image1 .and. allocated(outer)) stop 15
  if (image1 .and. allocated(dt_outer%i)) stop 16

end program test_nested_teams

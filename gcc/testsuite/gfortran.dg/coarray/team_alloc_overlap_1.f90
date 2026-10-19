! { dg-do run }
! { dg-skip-if "CHANGE TEAM needs a coarray library" { *-*-* } { "-fcoarray=single" } { "" } }
!
! Coarrays allocated in a child team must not overlap.  Each image's part is
! located by its index among all images, so the allocation has to provide a
! part for every image and not only for those of the team.

program team_alloc_overlap_1
  use iso_fortran_env, only: team_type
  implicit none
  integer, parameter :: m = 1000
  integer, allocatable :: a(:)[:], b(:)[:]
  type(team_type) :: t
  integer :: me

  me = this_image ()
  form team (merge (1, 2, mod (me, 2) == 1), t)
  change team (t)
    allocate (a(m)[*], b(m)[*])
    a = me
    b = -me
    sync all
    if (any (a /= me)) stop 1
    if (any (b /= -me)) stop 2
    deallocate (a, b)
  end team
end program team_alloc_overlap_1

!{ dg-do run }

! Check that team_number is supported in coindices.
! Adapted from code sent by Thomas Koenig  <tkoenig@gcc.gnu.org>

program pr98903
  use, intrinsic :: iso_fortran_env
  integer :: me, n, s
  integer :: a[*]
  type(team_type) :: team

  me = this_image()
  n = num_images()
  a = 42
  s = 42

  ! Checking against single image only.  Therefore team statements are
  ! not viable nor are they (yet) supported by GFortran.
  if (a[1, team_number=-1, stat=s] /= 42) stop 1
  if (s /= 0) stop 2

  s = 42
  if (a[1, team = team, stat=s] /= 42) stop 3
  if (s /= 0) stop 4

  s = 42
  if (a[1, stat=s] /= 42) stop 5
  if (s /= 0) stop 6
end program pr98903


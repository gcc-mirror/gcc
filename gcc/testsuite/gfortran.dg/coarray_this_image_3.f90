! { dg-do compile }
! { dg-options "-fcoarray=lib" }


use, intrinsic :: iso_fortran_env, only: team_type 
integer :: caf[*]
integer, allocatable :: res(:)
type(team_type) :: team

j1 = this_image()  ! ok
j1 = this_image('bar') !{ dg-error "First argument of 'this_image'" }
res = this_image(caf) ! ok
res = this_image(caf, caf) !{ dg-error "Second argument of 'this_image'" }
j2 = this_image(caf, 1) ! ok
j3 = this_image(caf, 'foo') !{ dg-error "Second argument of 'this_image'" }
j4 = this_image(caf, [1, 2]) !{ dg-error "Second argument of 'this_image'" }
j5 = this_image(team) ! ok
j6 = this_image(team, caf) !{ dg-error "Second argument of 'this_image'" }
res = this_image(caf, team) ! ok
res = this_image(caf, team, 'foo') !{ dg-error "shall be of type 'team_type'" }
j4 = this_image(caf, 1, team) ! ok
j5 = this_image(caf, 1, team, 'baz') !{ dg-error "Too many arguments in call" }
j6 = this_image(dim=1, team=team, coarray=caf)

!k1 = num_images()

!k2 = num_images(6)

!k3 = num_images(distance=7)

!k4 = num_images(distance=8, failed=.true.)

!k5 = num_images(failed=.false.)
end

!{ dg-do run }
!{ dg-options "-fdump-tree-original -fcoarray=single" }
!

use, intrinsic :: iso_fortran_env, only: team_type 
integer :: caf[2,*]
integer, allocatable :: res(:)
type(team_type) :: team

form team(1, team, new_index=MOD(this_image() + 43, num_images()) + 1)
j1 = this_image()
if (j1 /= 1) then
        print *, me, ":", j1
        stop 1
endif
res = this_image(caf)
if (any (res /= [1, 1])) then
        print *, me, ":", res
        stop 2
endif
j2 = this_image(caf, 1)
if (j2 /= 1) then
        print *, me, ":", j2
        stop 3
endif
j3 = this_image(team)
if (j3 /= MOD(this_image() + 43, num_images()) +1) then
        print *, me, ":", j3
        stop 4
endif
res = this_image(caf, team)
if (any(res /= [1, 1])) then
        print *, me, ":", res
        stop 5
endif
j4 = this_image(caf, 1, team)
if (j4 /= 1) then
        print *, me, ":", j4
        stop 6
endif
associate(me => this_image())
end associate
k1 = num_images()
k2 = num_images(team)
k3 = num_images(-1)
end

! { dg-final { scan-tree-dump-times "j\[1-4\] = 1;" 4 "original" } }
! { dg-final { scan-tree-dump-times "A\\.\[0-9\]+\\\[2\\\] = \\\{1, 1\\\};" 4 "original" } }
! { dg-final { scan-tree-dump "k1 = 1;" "original" } }
! { dg-final { scan-tree-dump "k2 = 1;" "original" } }
! { dg-final { scan-tree-dump "k3 = 1;" "original" } }

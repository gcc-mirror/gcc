! { dg-do run }
!
! PR fortran/53526
!
! Check handling of move_alloc with coarrays
!
implicit none
integer, allocatable :: u[:], v[:], w(:)[:,:], x(:)[:,:]

allocate (u[4:*])
call move_alloc (u, v)
if (allocated (u)) STOP 1
if (lcobound (v, dim=1) /= 4) STOP 2
if (ucobound (v, dim=1) /= 3 + num_images()) STOP 3

allocate (w(-2:3)[4:5,-1:*])
call move_alloc (w, x)
if (allocated (w)) STOP 4
if (lbound (x, dim=1) /= -2) STOP 5
if (ubound (x, dim=1) /= 3) STOP 6
if (any (lcobound (x) /= [4, -1])) STOP 7
if (any (ucobound (x) /= [5, -2 + (num_images()+1)/2])) STOP 8

end

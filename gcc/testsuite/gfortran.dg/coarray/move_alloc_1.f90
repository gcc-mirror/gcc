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
if (allocated (u)) call abort ()
if (lcobound (v, dim=1) /= 4) call abort ()
if (ucobound (v, dim=1) /= 3 + num_images()) call abort ()

allocate (w(-2:3)[4:5,-1:*])
call move_alloc (w, x)
if (allocated (w)) call abort ()
if (lbound (x, dim=1) /= -2) call abort ()
if (ubound (x, dim=1) /= 3) call abort ()
if (any (lcobound (x) /= [4, -1])) call abort ()
if (any (ucobound (x) /= [5, -2 + (num_images()+1)/2])) call abort ()

end

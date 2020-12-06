! { dg-do run }
! { dg-set-target-env-var GFORTRAN_NUM_IMAGES "2" }
! Allocatable scalar corrays were mishandled (ICE)
!
type t
  integer, allocatable :: caf[:]
end type t
type(t) :: a
allocate (a%caf[3:*])
a%caf = 7
if (a%caf /= 7) STOP 1
if (any (lcobound (a%caf) /= [ 3 ]) &
    .or. ucobound (a%caf, dim=1) /= num_images ()+2)  &
  STOP 2
deallocate (a%caf)
end

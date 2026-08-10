! { dg-do run }
!
! PR fortran/103474
!
! F2018:5.4.7(5) - a subobject of a coarray is a coarray with the
! codimensions of that coarray.  Check LCOBOUND, UCOBOUND, COSHAPE,
! THIS_IMAGE and IMAGE_INDEX applied to a component of a coarray.
!
! Contributed by G. Steinmetz  <gscfq@t-online.de>

program cobounds_subobject_1
  type t
    integer :: a
    integer :: b(2)
    integer :: c(1,2)
  end type

  type(t), save          :: u[2,3:*]
  type(t), save          :: v(4)[5:*]
  type(t), allocatable   :: y[:,:]
  class(t), allocatable  :: x[:]
  class(t), allocatable  :: z(:)[:]

  ! Explicit cobounds are simplified at compile time.
  if (lcobound (u%a, dim=1)    /= 1) stop 1
  if (lcobound (u%b, dim=2)    /= 3) stop 2
  if (ucobound (u%b, dim=1)    /= 2) stop 3
  if (lcobound (v(1)%b, dim=1) /= 5) stop 4

  if (any (lcobound (u%c)      /= lcobound (u)))   stop 5
  if (any (lcobound (v(2)%b)   /= lcobound (v)))   stop 6
  if (any (ucobound (v(2)%b)   /= ucobound (v)))   stop 7
  if (any (this_image (u%b)    /= this_image (u))) stop 8
  if (any (coshape (u%c)       /= coshape (u)))    stop 9

  ! Deferred cobounds are resolved at run time.
  allocate (x[3:*])
  allocate (y[2,3:*])
  allocate (z(2)[3:*])

  if (any (lcobound (x%a)      /= lcobound (x)))   stop 10
  if (any (lcobound (x%b)      /= [3]))            stop 11
  if (any (ucobound (x%c)      /= ucobound (x)))   stop 12
  if (any (lcobound (y%b)      /= lcobound (y)))   stop 13
  if (any (ucobound (y%b)      /= ucobound (y)))   stop 14
  if (any (lcobound (z(1)%b)   /= lcobound (z)))   stop 15
  if (any (ucobound (z(1)%b)   /= ucobound (z)))   stop 16
  if (lcobound (y%c, dim=2)    /= lcobound (y, dim=2)) stop 17
  if (ucobound (y%c, dim=1)    /= ucobound (y, dim=1)) stop 18
  if (any (this_image (x%b)    /= this_image (x))) stop 19
  if (any (this_image (y%c)    /= this_image (y))) stop 20
  if (any (coshape (y%b)       /= coshape (y)))    stop 21
  if (image_index (y%b, [1,3]) /= image_index (y, [1,3])) stop 22
end program

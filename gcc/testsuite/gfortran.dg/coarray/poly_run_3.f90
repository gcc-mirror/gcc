! { dg-do run }
!
! Check that the bounds of polymorphic coarrays is
! properly handled.
!
type t
end type t
class(t), allocatable :: a(:)[:]
class(t), allocatable :: b[:], d[:]

allocate(a(1)[*])
if (this_image() == 1 .and. any (this_image(a) /= lcobound(a))) &
  call abort ()
if (any (lcobound(a) /= 1)) call abort()
if (any (ucobound(a) /= this_image())) call abort ()
deallocate(a)

allocate(b[*])
if (this_image() == 1 .and. any (this_image(b) /= lcobound(b))) &
  call abort ()
if (any (lcobound(b) /= 1)) call abort()
if (any (ucobound(b) /= this_image())) call abort ()
deallocate(b)

allocate(a(1)[-10:*])
if (this_image() == 1 .and. any (this_image(a) /= lcobound(a))) &
  call abort ()
if (any (lcobound(a) /= -10)) call abort()
if (any (ucobound(a) /= -11+this_image())) call abort ()
deallocate(a)

allocate(d[23:*])
if (this_image() == 1 .and. any (this_image(d) /= lcobound(d))) &
  call abort ()
if (any (lcobound(d) /= 23)) call abort()
if (any (ucobound(d) /= 22+this_image())) call abort ()
deallocate(d)

end

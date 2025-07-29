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
  STOP 1
if (any (lcobound(a) /= 1)) STOP 2
if (any (ucobound(a) /= this_image())) STOP 3
deallocate(a)

allocate(b[*])
if (this_image() == 1 .and. any (this_image(b) /= lcobound(b))) &
  STOP 4
if (any (lcobound(b) /= 1)) STOP 5
if (any (ucobound(b) /= this_image())) STOP 6
deallocate(b)

allocate(a(1)[-10:*])
if (this_image() == 1 .and. any (this_image(a) /= lcobound(a))) &
  STOP 7
if (any (lcobound(a) /= -10)) STOP 8
if (any (ucobound(a) /= -11+this_image())) STOP 9
deallocate(a)

allocate(d[23:*])
if (this_image() == 1 .and. any (this_image(d) /= lcobound(d))) &
  STOP 10
if (any (lcobound(d) /= 23)) STOP 11
if (any (ucobound(d) /= 22+this_image())) STOP 12
deallocate(d)

end

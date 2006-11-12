! { dg-do compile }
! Tests patch for PR29758, which arose from PR29431.  There was no check that there
! were enough elements in the source to match the shape.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
   real :: a(2,2), b = 1.0, c(3), d(4)
   a = reshape ([b], [2,2]) ! { dg-error "not enough elements" }
   a = reshape (c, [2,2])   ! { dg-error "not enough elements" }
   a = reshape (d, [2,2])
end

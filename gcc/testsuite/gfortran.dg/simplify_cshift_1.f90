! { dg-do run }
program foo

   implicit none
   
   type t
      integer i
   end type t

   type(t), parameter :: d(5) = [t(1), t(2), t(3), t(4), t(5)]
   type(t) e(5), q(5)

   integer, parameter :: a(5) = [1, 2, 3, 4, 5]
   integer i, b(5), c(5), v(5)

   c = [1, 2, 3, 4, 5]

   b = cshift(a, -2)
   v = cshift(c, -2)
   if (any(b /= v)) STOP 1

   b = cshift(a, 2)
   v = cshift(c, 2)
   if (any(b /= v)) STOP 2

   ! Special cases shift = 0, size(a), -size(a)
   b = cshift([1, 2, 3, 4, 5], 0)
   if (any(b /= a)) STOP 3
   b = cshift([1, 2, 3, 4, 5], size(a))
   if (any(b /= a)) STOP 4
   b = cshift([1, 2, 3, 4, 5], -size(a))
   if (any(b /= a)) STOP 5

   ! simplification of array arg.
   b = cshift(2 * a, 0)
   if (any(b /= 2 * a)) STOP 6

   ! An array of derived types works too.
   e = [t(1), t(2), t(3), t(4), t(5)]
   e = cshift(e, 3)
   q = cshift(d, 3)
   do i = 1, 5
      if (e(i)%i /= q(i)%i) STOP 7
   end do

end program foo

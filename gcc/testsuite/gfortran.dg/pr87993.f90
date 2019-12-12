! { dg-do run }
! Code contributed by Gerhard Steinmetz <gscfq at t-online dot de>
program p
   integer, parameter :: a(2) = 1
   integer, parameter :: b = a%kind
   if (any(a /= 1)) stop 1
   if (b /= kind(a)) stop 2
end

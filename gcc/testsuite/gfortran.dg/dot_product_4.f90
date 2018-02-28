! { dg-do run }
! PR fortran/83998
program p
   integer, parameter :: a(0) = 1
   real, parameter :: b(0) = 1
   complex, parameter :: c(0) = 1
   logical, parameter :: d(0) = .true.
   if (dot_product(a,a) /= 0) STOP 1
   if (dot_product(b,b) /= 0) STOP 2
   if (dot_product(c,c) /= 0) STOP 3
   if (dot_product(d,d) .neqv. .false.) STOP 4
end


! { dg-do run }
program p
   integer, parameter :: a(3,2) = 1
   real, parameter :: b(2,3) = 2
   real d(3,3)
   d = 4
   if (any(d /= matmul(a,b))) STOP 1
end

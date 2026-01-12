! { dg-do compile }
module m
   integer :: i, j
   integer, parameter :: a(3) = [1, 2, 3]
   integer, parameter :: b(3) = [(a(j), i=1,3)] ! { dg-error "Expecting constant expression" }
end

! { dg-do compile }
! Test of fix to PR19754
program PR19754_1
   real x(3,3),y(2,2)
   x = 1.
   y = 2.
   x = x + y ! { dg-error "Shapes for operands at" }
end program PR19754_1


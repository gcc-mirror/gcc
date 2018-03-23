! { dg-do run }
program foo
   real, parameter :: x(3) = 2.0 * [real :: 1, 2, 3 ]
   if (any(x /= [2., 4., 6.])) STOP 1
end program foo

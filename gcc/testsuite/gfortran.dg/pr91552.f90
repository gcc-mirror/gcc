! { dg-do run }
! PR fortran/91552
! Code contributed by Gerhard Steinmetz.
program p
   real :: y(3), z(4)
   y = 2.0 * [real :: 1, [2], 3]
   z = 2.0 * [real :: 1, [2, [4]], 3]
   if (any(y /= [2., 4., 6.])) stop 1
   if (any(z /= [2., 4., 8., 6.])) stop 2
end

! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/67615
!
program foo

   implicit none

   integer i(2), j
   real x
   complex z
 
   j = 2
   if (j) 10, 20, 30

   x = -1
   if (x) 10, 20, 30

   z = (1,2)
   if (z) 10, 20, 30                   ! { dg-error "Arithmetic IF statement" }

   i = [1, 2]
   if (i) 10, 20, 30                   ! { dg-error "Arithmetic IF statement" }

   if ( [1] ) 10, 20, 30               ! { dg-error "Arithmetic IF statement" }
   if ( [1, -1] ) 10, 20, 30           ! { dg-error "Arithmetic IF statement" }
   if ( [real :: 1, -1] ) 10, 20, 30   ! { dg-error "Arithmetic IF statement" }

10 stop
20 stop
30 stop

end program foo

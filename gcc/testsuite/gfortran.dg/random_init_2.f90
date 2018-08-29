! { dg-do run } 
program foo

   real x(2), y(2)

   call random_init(.false., .false.)
   call random_number(x)
!   print *, x
   x = int(1e6*x)

   call random_init(.false., .false.)
   call random_number(y)
!   print *, y
   y = int(1e6*y)

   if (any(x == y)) call abort

   call random_init(.true., .false.)
   call random_number(x)
!   print *, x
   x = int(1e6*x)

   call random_init(.true., .false.)
   call random_number(y)
!   print *, y
   y = int(1e6*y)

   if (any(x /= y)) call abort   

end program foo

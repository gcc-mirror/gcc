! { dg-do run }
program foo

   implicit none

   integer, allocatable :: x
   integer, allocatable :: a(:)

   logical a1, a2

   a1 = allocated(scalar=x)
   if (a1 .neqv. .false.) stop 1
   a2 = allocated(array=a)
   if (a2 .neqv. .false.) stop 2

   allocate(x)
   allocate(a(2))

   a1 = allocated(scalar=x)
   if (a1 .neqv. .true.) stop 3
   a2 = allocated(array=a)
   if (a2 .neqv. .true.) stop 4

end program foo

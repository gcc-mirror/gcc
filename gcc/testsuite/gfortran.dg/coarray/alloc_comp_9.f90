!{ dg-do run }

! Check PR85002 is fixed.
! Contributed by G. Steinmetz  <gscfq@t-online.de>

program pr85002
   type t
      integer, allocatable :: a(:)
   end type
   type t2
      type(t), allocatable :: b(:)
   end type
   type(t) :: x
   type(t2) :: y(2)[*]

   allocate (x%a(2))
   x%a = 123
   y = t2([x])

   if (.not. all((/(allocated(y(i)%b), i=1, 2)/))) stop 1
   if (any ((/(y(i)%b(1)%a /= 123, i=1,2)/))) stop 2
end


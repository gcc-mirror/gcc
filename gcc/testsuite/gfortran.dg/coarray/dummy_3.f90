!{ dg-do run }

! Check that PR77871 is fixed.

! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>

program pr77871
   type t
      integer :: i
   end type

   type(t) :: p[*]
   type(t), allocatable :: p2(:)[:]

   p%i = 42
   allocate (p2(5)[*])
   p2(:)%i = (/(i, i=0, 4)/)
   call s(p, 1)
   call s2(p2, 1)
contains
   subroutine s(x, n)
      class(t) :: x[*]
      integer :: n
      if (x[n]%i /= 42) stop 1
   end

   subroutine s2(x, n)
      class(t) :: x(:)[*]
      integer :: n
      if (any(x(:)[n]%i /= (/(i, i= 0, 4)/) )) stop 2
   end
end


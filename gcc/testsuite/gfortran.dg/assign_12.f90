!{ dg-do run }
!
! Check assignment works for derived types to memory referenced by pointer
! Contributed by G. Steinmetz  <gscfq@t-online.de>

program pr103391
   type t
     character(1) :: c
   end type
   type t2
      type(t), pointer :: a(:)
   end type

   type(t), target :: arr(2)
   type(t2) :: r

   arr = [t('a'), t('b')]

   r = f([arr])
   if (any(r%a(:)%c /= ['a', 'b'])) stop 1
contains
   function f(x)
      class(t), intent(in), target :: x(:)
      type(t2) :: f
      allocate(f%a(size(x,1)))
      f%a = x
   end
end

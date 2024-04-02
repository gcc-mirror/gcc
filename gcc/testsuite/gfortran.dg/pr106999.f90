! { dg-do compile }
! Test the fix for PR106999
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
program p
   type t
      integer :: i
      procedure(g), pointer :: f
   end type
   class(t), allocatable :: y, z
   procedure(g), pointer :: ff
   allocate (z)
   z%i = 42
   z%f => g
   ff => g
   call r(z%f)
   call s(z%f) ! { dg-error "Interface mismatch in dummy procedure" }
   call s(ff)  ! { dg-error "Interface mismatch in dummy procedure" }
contains
   subroutine g(x)
      class(t) :: x
      x%i = 84
   end
   subroutine r(x)
      procedure(g) :: x
      print *, "in r"
      allocate (y)
      call x(y)
      print *, y%i
   end
   subroutine s(x)
      class(*) :: x
   end subroutine
end

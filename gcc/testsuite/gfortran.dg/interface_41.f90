! { dg-do compile }
! PR fortran/85001 
! Contributed by Gerhard Steinmetz.
program p
   type t
   end type
   call s
contains
   real function f(x)
      class(t) :: x
      dimension :: x(:)
      f = 1.0
   end
   subroutine s
      type(t) :: x(2)
      real :: z
      z = f(x)     ! { dg-error "Rank mismatch in argument" }
   end
end

! { dg-do compile }
! PF fortran/68544
program p
   real x
   type t
   end type
   x = f(t)             ! { dg-error "used as an actual argument" }
end
subroutine b
   type t
   end type
   print *, shape(t)    ! { dg-error "used as an actual argument" }
end

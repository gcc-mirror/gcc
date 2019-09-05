! { dg-do compile }
! PR fortran/91660
! Code contributed by Gerhard Steinmetz
program p
   type t
   end type
   type (t x    ! { dg-error "Malformed type-spec" }
   x = t()      ! { dg-error "Can't convert" }
end

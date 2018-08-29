! { dg-do compile }
! PR fortran/85687
! Code original contributed by Gerhard Steinmetz gscfq at t-oline dot de
program p
   type t
   end type
   print *, rank(t)  ! { dg-error "must be a data object" }
end

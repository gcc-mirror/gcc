! { dg-do compile }
! PR fortran/65173
program p
   type t
      character(*), allocatable :: x(*)  ! { dg-error "must have a deferred shape" }
   end type                              ! { dg-error "needs to be a constant specification" "" { target "*-*-*" } .-1 } 
end

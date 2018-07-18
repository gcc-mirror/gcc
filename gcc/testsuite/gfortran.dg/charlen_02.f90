! { dg-do compile }
! PR fortran/65173
program p
   type t
      character(1), allocatable :: n(256) ! { dg-error "must have a deferred shape" }
   end type
end

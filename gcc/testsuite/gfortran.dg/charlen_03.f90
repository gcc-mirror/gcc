! { dg-do compile }
! PR fortran/65173
program p
   type t
      character(:), allocatable :: x(n) ! { dg-error "must have a deferred shape" }
   end type
end
! { dg-excess-errors "must be of INTEGER type" }


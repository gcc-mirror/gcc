! { dg-do compile }
! PR fortran/69859
program p
   type t
      character(2), allocatable :: a(*) ! { dg-error "must have a deferred shape" }
      character(*), allocatable :: b(2) ! { dg-error "must have a deferred shape" }
      character(*), allocatable :: c(*) ! { dg-error "must have a deferred shape" }
   end type
end
! { dg-excess-errors "needs to be a constant specification" } 

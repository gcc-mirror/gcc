! { dg-do compile }
!
! PR fortran/78240
!
! Test a regression where an ICE occurred by passing an invalid reference
! to the error handling routine for non-constant array-specs in DATA list
! initializers.
!

program p
  integer x(n)    /1/   ! { dg-error "Nonconstant array" }
end

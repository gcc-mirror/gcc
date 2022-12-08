! { dg-do compile }
! PR fortran/108025

subroutine foo (x)
  real, contiguous :: x(:)
  contiguous       :: x    ! { dg-error "Duplicate CONTIGUOUS attribute" }
end

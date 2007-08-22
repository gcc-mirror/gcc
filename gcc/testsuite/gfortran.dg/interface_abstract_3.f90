! { dg-do compile }
! test for C1204 of Fortran 2003 standard:
! module procedure not allowed in abstract interface
module m
  abstract interface
     module procedure p	! { dg-error "must be in a generic module interface" }
  end interface
contains
  subroutine p()
  end subroutine
end module m

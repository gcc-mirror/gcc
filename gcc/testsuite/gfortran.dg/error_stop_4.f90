! { dg-do compile }
! { dg-options "-std=f2008" }
!
! F2018 permits ERROR STOP in PURE procedures
!
pure subroutine foo()
  error stop "failed"  ! { dg-error "Fortran 2018: ERROR STOP statement at .1. in PURE procedure" }
end

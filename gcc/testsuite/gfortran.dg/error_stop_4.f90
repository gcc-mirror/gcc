! { dg-do compile }
! { dg-options "-std=f2008ts" }
!
! F2015 permits ERROR STOP in PURE procedures
! FIXME: Change to error_stop_3.f90 to -std=f2015.
!
pure subroutine foo()
  error stop "failed"  ! { dg-error "GNU Extension: ERROR STOP statement at .1. in PURE procedure" }
end

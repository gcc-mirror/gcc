! { dg-do compile }
! { dg-options "-std=gnu" }
!
! F2015 permits ERROR STOP in PURE procedures
! FIXME: Change to -std=f2015, when available
!
pure subroutine foo()
  error stop "failed"
end

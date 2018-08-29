! { dg-do compile }
! { dg-options "-std=f2018" }
!
! F2018 permits ERROR STOP in PURE procedures
!
pure subroutine foo()
  error stop "failed"
end

! { dg-do run }
! { dg-options "-fbounds-check" }
!
! Testcase for PR30655, we used to issue a compile-time warning
  integer i(12), j
  j = -1
  i(0:j) = 42
  end

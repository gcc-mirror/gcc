! { dg-do run }
! { dg-options "-fno-range-check -O0" }
!
! This testcase arose from PR 31262
  integer :: a
  integer(kind=8) :: b
  b = -huge(b) / 7
  b = 7894_8 * b - 78941_8
  if (7894_8 * (-huge(b) / 7) - 78941_8 /= b) STOP 2

  a = 1234789786453123
  if (a - 1234789786453123 /= a - (-426244989)) STOP 3
  end

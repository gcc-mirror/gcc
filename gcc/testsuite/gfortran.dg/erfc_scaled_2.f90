! { dg-do compile }
!
! Check that ERFC_SCALED can be used in initialization expressions
  real, parameter :: r = 100*erfc_scaled(12.7)
  integer(kind=int(r)) :: i

  real(kind=8), parameter :: r8 = 100*erfc_scaled(6.77)
  integer(kind=int(r8)) :: j

  i = 12
  j = 8
  print *, i, j

  end

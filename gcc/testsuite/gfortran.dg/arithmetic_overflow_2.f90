! { dg-do compile }
! { dg-additional-options "-frange-check" }
!
! PR fortran/113799 - handle arithmetic overflow on unary minus

program p
  implicit none
  real, parameter :: inf = real(z'7F800000')
  real, parameter :: someInf(*) = [inf, 0.]
  print *, -someInf         ! { dg-error "Arithmetic overflow" }
  print *, minval(-someInf) ! { dg-error "Arithmetic overflow" }
end

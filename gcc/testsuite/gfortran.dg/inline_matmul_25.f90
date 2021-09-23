! { dg-do compile }
! { dg-options "-ffrontend-optimize" }
! PR fortran/99839 - ICE in inline_matmul_assign

program p
  real :: x(3, 3) = 1.0
  class(*), allocatable :: z(:, :)
  z = matmul(x, x)
end

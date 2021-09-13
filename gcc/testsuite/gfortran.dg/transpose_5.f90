! { dg-do compile }
! { dg-options "-O2" }
! PR fortran/99840 - ICE in gfc_simplify_matmul, at fortran/simplify.c:4777
program p
  integer, parameter :: x(0,0) = 0
  integer :: y(0,0)
  y = matmul (x, transpose(x))
end

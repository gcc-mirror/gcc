! PR fortran/92977
! { dg-do compile }
! { dg-additional-options "-O2" }

program pr92977
  integer :: n = 1
  integer :: a
!$omp atomic write
  a = f(n) - f(n)
contains
  integer function f(x)
    integer, intent(in) :: x
    f = x
  end
end

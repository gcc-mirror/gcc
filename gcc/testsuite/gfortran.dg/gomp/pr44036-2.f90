! PR fortran/44036
! { dg-do compile }
! { dg-options "-fopenmp" }
subroutine foo(a, b)
  integer, external :: a
  integer, external, pointer :: b
  integer, external :: c
  integer, external, pointer :: d
  integer :: x
  d => a
!$omp parallel default(none) private (x) firstprivate (b, d)
  x = a(4)
  x = b(5)
  x = c(6)
  x = d(7)
!$omp end parallel
end

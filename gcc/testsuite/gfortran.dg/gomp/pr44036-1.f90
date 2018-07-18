! PR fortran/44036
! { dg-do compile }
! { dg-options "-fopenmp" }
subroutine foo(a, b)
  integer, external :: a
  integer, external, pointer :: b
  integer, external :: c
  integer, external, pointer :: d
  integer :: x
  x = 6
!$omp parallel default(none) private (x)
  x = a(4)
!$omp end parallel
!$omp parallel default(none) private (x)	! { dg-error "enclosing 'parallel'" }
  x = b(5)					! { dg-error "not specified in" }
!$omp end parallel
!$omp parallel default(none) private (x)
  x = c(6)
!$omp end parallel
  d => a
!$omp parallel default(none) private (x)	! { dg-error "enclosing 'parallel'" }
  x = d(7)					! { dg-error "not specified in" }
!$omp end parallel
end

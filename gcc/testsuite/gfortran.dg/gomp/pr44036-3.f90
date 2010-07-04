! PR fortran/44036
! { dg-do compile }
! { dg-options "-fopenmp" }
subroutine foo(a)
  integer, external :: a, c
  integer :: x
!$omp parallel default(none) private (x) shared (a)	! { dg-error "is not a variable" }
  x = a(6)
!$omp end parallel
!$omp parallel default(none) private (x) shared (c)	! { dg-error "is not a variable" }
  x = c(6)
!$omp end parallel
end

! PR fortran/77973
! { dg-do compile }

subroutine s(x)
  integer :: x(:)
  integer :: i
!$omp parallel
!$omp target
  x(1) = 1
!$omp end target
!$omp end parallel
end

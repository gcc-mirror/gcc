! PR fortran/77352
! { dg-do compile }
! { dg-additional-options "-fstack-arrays -O2" }
! { dg-additional-options "-fopenacc" { target fopenacc } }

program pr77352
  real, allocatable :: a(:,:), b(:)
  integer :: m, n
  m = 4
  n = 2
  allocate (a(m,n), b(m))
  a = 1.0
!$omp parallel workshare
  b(:) = [ sum(a, dim=1) ]
!$omp end parallel workshare
end

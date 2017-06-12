! { dg-do compile }
! { dg-options "-fopenmp -fcray-pointer" }

  integer :: a, b
  pointer (ip, a)

  b = 2
  ip = loc (b)
!$omp parallel default (none) shared (ip)
  a = 1
!$omp end parallel

!$omp parallel default (none) private (ip, b)
  b = 3
  ip = loc (b)
  a = 1
!$omp end parallel

!$omp parallel default (none)	! { dg-error "enclosing 'parallel'" }
  a = 1		! { dg-error "'ip' not specified in enclosing 'parallel'" }
!$omp end parallel
end

! { dg-do compile }
! { dg-options "-fopenmp -fcray-pointer" }

subroutine foo (n)
  integer :: a, b (38), n
  pointer (ip, a (n + 1))

  b = 2
  n = 36
  ip = loc (b)
!$omp parallel default (none) shared (ip)
!$omp parallel default (none) shared (ip)
  a = 1
!$omp end parallel
!$omp end parallel

!$omp parallel default (none)
!$omp parallel default (none) private (ip, b)
  b = 3
  ip = loc (b)
  a = 1
!$omp end parallel
!$omp end parallel
end

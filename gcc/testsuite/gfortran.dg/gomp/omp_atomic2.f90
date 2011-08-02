  real :: r1, r2
  complex :: c1, c2
  integer :: i1, i2
!$omp atomic write
  c1 = 0
!$omp atomic write
  r2 = 0
!$omp atomic write
  i2 = 0
!$omp atomic read
  r1 = c1
!$omp atomic read
  c2 = r2
!$omp atomic read
  i1 = r2
!$omp atomic read
  c2 = i2
!$omp atomic write
  c1 = r1
!$omp atomic write
  r2 = c2
!$omp atomic write
  r2 = i1
!$omp atomic write
  i2 = c2
!$omp end atomic
!$omp atomic write
  c1 = 1 + 2 + r1
!$omp atomic write
  r2 = c2 + 2 + 3
!$omp atomic write
  r2 = 3 + 4 + i1
!$omp atomic write
  i2 = c2 + 4 + 5
!$omp atomic
  c1 = c1 * 2.
!$omp atomic update
  r2 = r2 / 4
!$omp end atomic
!$omp atomic update
  i2 = i2 + 8
!$omp atomic capture
  c1 = c1 * 2.
  r1 = c1
!$omp end atomic
!$omp atomic capture
  c2 = r2
  r2 = r2 / 4
!$omp end atomic
!$omp atomic capture
  i2 = i2 + 8
  c2 = i2
!$omp end atomic
end

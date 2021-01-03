module m
  integer a, b
end module m

subroutine f3 (c, d)
  use m
  implicit none
  integer i, c(64), d(64)
  !$omp parallel reduction (inscan, +: a)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
    ! ...
  !$omp end parallel
  !$omp sections reduction (inscan, +: a)  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" }
    !$omp section
    ! ...
  !$omp end sections
end

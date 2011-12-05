! PR middle-end/27573
! { dg-do compile }
! { dg-require-profiling "-fprofile-generate" }
! { dg-options "-O2 -fopenmp -fprofile-generate" }

program pr27573
  integer i,j
  j = 8
  !$omp parallel
    print *, "foo"
    do i = 1, j - 1
    end do
  !$omp end parallel
end

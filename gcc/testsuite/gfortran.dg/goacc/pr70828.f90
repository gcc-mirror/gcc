! Ensure that pointer mappings are preserved in nested parallel
! constructs.

! { dg-additional-options "-fdump-tree-gimple" }

program test
  integer, parameter :: n = 100
  integer i, data(n)

  data(:) = 0

  !$acc data copy(data(5:n-10))
  !$acc parallel loop
  do i = 10, n - 10
     data(i) = i
  end do
  !$acc end parallel loop
  !$acc end data
end program test

! { dg-final { scan-tree-dump-times "omp target oacc_data map\\(tofrom:data\\\[\_\[0-9\]+\\\] \\\[len: _\[0-9\]+\\\]\\) map\\(alloc:data \\\[pointer assign, bias: _\[0-9\]+\\\]\\)" 1 "gimple" } }
! { dg-final { scan-tree-dump-times "omp target oacc_parallel map\\(force_present:data\\\[D\\.\[0-9\]+\\\] \\\[len: D\\.\[0-9\]+\\\]\\) map\\(alloc:data \\\[pointer assign, bias: D\\.\[0-9\]+\\\]\\)" 1 "gimple" } }

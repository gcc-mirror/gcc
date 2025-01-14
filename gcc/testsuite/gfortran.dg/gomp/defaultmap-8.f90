! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program main
  implicit none
  integer, parameter :: N = 1000
  integer :: a(N), b(N), c(N), i
  
  ! Should generate implicit 'map(present, alloc)' clauses.
  !$omp target defaultmap (present: aggregate)
    do i = 1, N
      c(i) = a(i) + b(i)
    end do
  !$omp end target

  ! Should generate implicit 'map(present, alloc)' clauses,
  ! and they should go before other non-present clauses.
  !$omp target map(from: c) defaultmap (present: aggregate)
    do i = 1, N
      c(i) = a(i) + b(i)
    end do
  !$omp end target
end program
  
! { dg-final { scan-tree-dump "pragma omp target.*defaultmap\\(present:aggregate\\).*map\\(force_present:c \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\) map\\(force_present:b \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\)" "gimple" } }
! { dg-final { scan-tree-dump "pragma omp target.*map\\(force_present:b \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\] \\\[runtime_implicit\\\]\\) map\\(from:c \\\[len: \[0-9\]+\\\]\\) defaultmap\\(present:aggregate\\)" "gimple" } }

! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program main
  implicit none
  integer, parameter :: N = 1000
  integer :: a(N), b(N), c(N), i

  ! Should be able to parse 'present' map modifier.
  !$omp target enter data map (present, to: a, b)

  !$omp target data map (present, to: a, b) map (always, present, from: c)
    !$omp target map (present, to: a, b) map (present, from: c)
      do i = 1, N
        c(i) = a(i) + b(i)
      end do
    !$omp end target
  !$omp end target data

  !$omp target exit data map (always, present, from: c)

  ! Map clauses with 'present' modifier should go ahead of those without.
  !$omp target map (to: a) map (present, to: b) map (from: c)
    do i = 1, N
      c(i) = a(i) + b(i)
    end do
  !$omp end target
end program

! { dg-final { scan-tree-dump "pragma omp target enter data map\\(force_present:a \\\[len: \[0-9\]+\\\]\\) map\\(force_present:b \\\[len: \[0-9\]+\\\]\\)" "gimple" } }
! { dg-final { scan-tree-dump "pragma omp target data map\\(force_present:a \\\[len: \[0-9\]+\\\]\\) map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(always,present,from:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } }
! { dg-final { scan-tree-dump "pragma omp target.*map\\(force_present:a \\\[len: \[0-9\]+\\\]\\) map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(force_present:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } }
! { dg-final { scan-tree-dump "pragma omp target exit data map\\(always,present,from:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } }
! { dg-final { scan-tree-dump "pragma omp target.*map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(to:a \\\[len: \[0-9\]+\\\]\\) map\\(from:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } }

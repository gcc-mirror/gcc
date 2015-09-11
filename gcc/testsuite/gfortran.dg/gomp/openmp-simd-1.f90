! { dg-do compile }
! { dg-options "-fno-openmp -fopenmp-simd -fdump-tree-original -O2" }

!$omp declare reduction (foo:integer:omp_out = omp_out + omp_in)
  interface
    integer function foo (x, y)
      integer, value :: x, y
!$omp declare simd (foo) linear (y : 2)
    end function foo
  end interface
  integer :: i, a(64), b, c
  integer, save :: d
!$omp threadprivate (d)
  d = 5
  a = 6
!$omp simd
  do i = 1, 64
    a(i) = foo (a(i), 2 * i)
  end do
  b = 0
  c = 0
!$omp simd reduction (+:b) reduction (foo:c)
  do i = 1, 64
    b = b + a(i)
    c = c + a(i) * 2
  end do
  print *, b
  b = 0
!$omp parallel
!$omp do simd schedule(static, 4) safelen (8) reduction (+:b)
  do i = 1, 64
    a(i) = a(i) + 1
    b = b + 1
  end do
!$omp end parallel
  print *, b
  b = 0
!$omp parallel do simd schedule(static, 4) safelen (8) &
!$omp num_threads (4) if (.true.) reduction (+:b)
  do i = 1, 64
    a(i) = a(i) + 1
    b = b + 1
  end do
  print *, b
  b = 0
!$omp parallel
!$omp do simd schedule(static, 4) safelen (8) reduction (+:b)
  do i = 1, 64
    a(i) = a(i) + 1
    b = b + 1
  end do
!$omp enddosimd
!$omp end parallel
  print *, b
  b = 0
!$omp parallel do simd schedule(static, 4) safelen (8) &
!$omp num_threads (4) if (.true.) reduction (+:b)
  do i = 1, 64
    a(i) = a(i) + 1
    b = b + 1
  end do
!$omp end parallel do simd
!$omp atomic seq_cst
  b = b + 1
!$omp end atomic
!$omp barrier
!$omp parallel private (i)
!$omp cancellation point parallel
!$omp critical (bar)
  b = b + 1
!$omp end critical (bar)
!$omp flush(b)
!$omp single
  b = b + 1
!$omp end single
!$omp do ordered
  do i = 1, 10
    !$omp atomic
    b = b + 1
    !$omp end atomic
    !$omp ordered
      print *, b
    !$omp end ordered
  end do
!$omp end do
!$omp master
  b = b + 1
!$omp end master
!$omp cancel parallel
!$omp end parallel
!$omp parallel do schedule(runtime) num_threads(8)
  do i = 1, 10
    print *, b
  end do
!$omp end parallel do
!$omp sections
!$omp section
  b = b + 1
!$omp section
  c = c + 1
!$omp end sections
  print *, b
!$omp parallel sections firstprivate (b) if (.true.)
!$omp section
  b = b + 1
!$omp section
  c = c + 1
!$omp endparallelsections
!$omp workshare
  b = 24
!$omp end workshare
!$omp parallel workshare num_threads (2)
  b = b + 1
  c = c + 1
!$omp end parallel workshare
  print *, b
!$omp parallel
!$omp single
!$omp taskgroup
!$omp task firstprivate (b)
  b = b + 1
!$omp taskyield
!$omp end task
!$omp task firstprivate (b)
  b = b + 1
!$omp end task
!$omp taskwait
!$omp end taskgroup
!$omp end single
!$omp end parallel
  print *, a, c
end

! { dg-final { scan-tree-dump-times "pragma omp simd" 6 "original" } }
! { dg-final { scan-tree-dump-times "pragma omp" 6 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP" 0 "original" } }

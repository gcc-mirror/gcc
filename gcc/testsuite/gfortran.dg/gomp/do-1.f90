! { dg-do compile }
! { dg-options "-O -fopenmp -fdump-tree-omplower" }

subroutine foo (i, j, k, s, a)
  integer :: i, j, k, s, a(100), l
!$omp parallel do schedule (dynamic, s * 2)
  do 100, l = j, k
100 a(l) = i
!$omp parallel do schedule (dynamic, s * 2)
  do 101, l = j, k, 3
101 a(l) = i + 1
end subroutine foo

subroutine bar (i, j, k, s, a)
  integer :: i, j, k, s, a(100), l
!$omp parallel do schedule (guided, s * 2)
  do 100, l = j, k
100 a(l) = i
!$omp parallel do schedule (guided, s * 2)
  do 101, l = j, k, 3
101 a(l) = i + 1
end subroutine bar

! { dg-final { scan-tree-dump-times "GOMP_parallel_loop_dynamic_start" 2 "omplower" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-times "GOMP_parallel_loop_guided_start" 2 "omplower" { xfail *-*-* } } }

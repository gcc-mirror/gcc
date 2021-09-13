! { dg-additional-options "-fdump-tree-original" }

implicit none
integer :: q, i, j
integer :: r
r = 0
!$omp loop bind(thread) reduction(default,+: r) collapse(2) order(concurrent), private(q) lastprivate(i)
do i = 1,4
do j = 1,4
  r = r + 1
  q = 5
end do
end do

!$omp teams loop bind(teams) collapse(2) order(concurrent), private(q) lastprivate(i) reduction(default,+: r) 
do i = 1,4
do j = 1,4
  r = r + 1
  q = 5
end do
end do

!$omp target teams loop bind(thread) reduction(+: r) collapse(2) order(concurrent), private(q) lastprivate(i)
do i = 1,4
do j = 1,4
  r = r + 1
  q = 5
end do
end do

!$omp parallel loop bind(thread) collapse(2) order(concurrent), private(q) lastprivate(i) reduction(default,+: r) 
do i = 1,4
do j = 1,4
  r = r + 1
  q = 5
end do
end do

!$omp target parallel loop bind(parallel) collapse(2) order(concurrent), private(q) lastprivate(i) reduction(default,+: r) 
do i = 1,4
do j = 1,4
  r = r + 1
  q = 5
end do
end do

end
 
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:i\\) map\\(tofrom:r\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel shared\\(i\\) shared\\(r\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams shared\\(i\\) shared\\(r\\)\[\r\n\]" 2 "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp loop private\\(q\\) lastprivate\\(i\\) reduction\\(\\+:r\\) order\\(concurrent\\) collapse\\(2\\) bind\\(parallel\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp loop private\\(q\\) lastprivate\\(i\\) reduction\\(\\+:r\\) order\\(concurrent\\) collapse\\(2\\) bind\\(teams\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp loop private\\(q\\) lastprivate\\(i\\) reduction\\(\\+:r\\) order\\(concurrent\\) collapse\\(2\\) bind\\(thread\\)" 3 "original" } }

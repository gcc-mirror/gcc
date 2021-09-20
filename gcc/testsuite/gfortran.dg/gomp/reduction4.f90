! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! (in_)reduction clause
! Test all in-principle valid combinations, even if
! not valid in this context (some fail at ME level)
!
implicit none
integer :: a, b, i
a = 0

! ------------ parallel ------------
!$omp parallel reduction(+:a)
do i=1,10
  a = a + 1
end do
!$omp end parallel

!$omp parallel reduction(default,+:a)
do i=1,10
  a = a + 1
end do
!$omp end parallel

!$omp parallel reduction(task,+:a)
do i=1,10
  a = a + 1
end do
!$omp end parallel


! ------------ simd ------------
!$omp simd reduction(+:a)
do i=1,10
  a = a + 1
end do

!$omp simd reduction(default,+:a)
do i=1,10
  a = a + 1
end do

!$omp simd reduction(task,+:a)  ! { dg-error "invalid 'task' reduction modifier on construct other than 'parallel', 'do', 'sections' or 'scope'" }
do i=1,10
  a = a + 1
end do

! ------------ do ------------
!$omp parallel
!$omp do reduction(+:a)
do i=1,10
  a = a + 1
end do
!$omp end parallel

!$omp parallel
!$omp do reduction(default,+:a)
do i=1,10
  a = a + 1
end do
!$omp end parallel

!$omp parallel
!$omp do reduction(task,+:a)
do i=1,10
  a = a + 1
end do
!$omp end parallel

! ------------ section ------------
!$omp parallel
!$omp sections reduction(+:a)
  !$omp section
  a = a + 1
!$omp end sections
!$omp end parallel

!$omp parallel
!$omp sections reduction(default,+:a)
  !$omp section
  a = a + 1
!$omp end sections
!$omp end parallel

!$omp parallel
!$omp sections reduction(task,+:a)
  !$omp section
  a = a + 1
!$omp end sections
!$omp end parallel

! ------------ task ------------
!$omp task in_reduction(+:a)
  a = a + 1
!$omp end task

! ------------ taskloop ------------
!$omp taskloop reduction(+:a) in_reduction(+:b)
do i=1,10
  a = a + 1
end do

!$omp taskloop reduction(default,+:a) in_reduction(+:b)
do i=1,10
  a = a + 1
end do

! ------------ target ------------
!$omp target in_reduction(+:b)
  a = a + 1
!$omp end target

! ------------ teams ------------
!$omp teams reduction(+:b)
  a = a + 1
!$omp end teams

!$omp teams reduction(default, +:b)
  a = a + 1
!$omp end teams

! ------------ taskgroup --------

!$omp taskgroup task_reduction(+:b)
  a = a + 1
!$omp end taskgroup

end

! { dg-final { scan-tree-dump-times "#pragma omp for reduction\\(\\\+:a\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp for reduction\\(task,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel\[\n\r\]" 6 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel private\\(i\\) reduction\\(\\\+:a\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel private\\(i\\) reduction\\(task,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp section\[\n\r\]" 3 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp sections reduction\\(\\\+:a\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp sections reduction\\(task,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) reduction\\(\\\+:a\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp simd linear\\(i:1\\) reduction\\(task,\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(always,tofrom:b\\) in_reduction\\(\\\+:b\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp task in_reduction\\(\\\+:a\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp teams reduction\\(\\\+:b\\)" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp taskloop reduction\\(\\\+:a\\) in_reduction\\(\\\+:b\\)" 2 "original" } }

! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

integer :: x, y

! EXEC_OMP_TARGET_TEAMS

!$omp target teams map(x) firstprivate(x)
x = x + 1
!$omp end target teams

!$omp target teams map(x) firstprivate(y)
x = y + 1
!$omp end target teams

! EXEC_OMP_TARGET_TEAMS_DISTRIBUTE

!$omp target teams distribute map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams distribute

!$omp target teams distribute map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target teams distribute

! EXEC_OMP_TARGET_TEAMS_LOOP

!$omp target teams loop map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams loop

!$omp target teams loop map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target teams loop

! EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD

!$omp target teams distribute simd map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams distribute simd

!$omp target teams distribute simd map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target teams distribute simd

! EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO

!$omp target teams distribute parallel do map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams distribute parallel do

!$omp target teams distribute parallel do map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target teams distribute parallel do

! EXEC_OMP_TARGET_PARALLEL

!$omp target parallel map(x) firstprivate(x)
x = x + 1
!$omp end target parallel

!$omp target parallel map(x) firstprivate(y)
x = y + 1
!$omp end target parallel

! EXEC_OMP_TARGET_PARALLEL_DO

!$omp target parallel do map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target parallel do

!$omp target parallel do map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target parallel do

! EXEC_OMP_TARGET_PARALLEL_LOOP

!$omp target parallel loop map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target parallel loop

!$omp target parallel loop map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target parallel loop

! EXEC_OMP_TARGET_PARALLEL_DO_SIMD

!$omp target parallel do simd map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target parallel do simd

!$omp target parallel do simd map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target parallel do simd

! EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD

!$omp target teams distribute parallel do simd map(x) firstprivate(x)
do i=1,1
  x = x + 1
end do
!$omp end target teams distribute parallel do simd

!$omp target teams distribute parallel do simd map(x) firstprivate(y)
do i=1,1
  x = y + 1
end do
!$omp end target teams distribute parallel do simd

! { dg-final { scan-tree-dump-times {omp target map\(tofrom:x\)} 10 "original" } }
! { dg-final { scan-tree-dump-times {omp target firstprivate\(y\) map\(tofrom:x\)} 10 "original" } }

! { dg-final { scan-tree-dump-times {omp teams firstprivate\(x\)} 6 "original" } }
! { dg-final { scan-tree-dump-times {omp teams firstprivate\(y\)} 6 "original" } }

! { dg-final { scan-tree-dump-times {omp parallel firstprivate\(x\)} 6 "original" } }
! { dg-final { scan-tree-dump-times {omp parallel firstprivate\(y\)} 6 "original" } }

end

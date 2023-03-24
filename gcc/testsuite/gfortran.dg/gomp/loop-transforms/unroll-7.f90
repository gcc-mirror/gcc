! { dg-additional-options "--param=omp-unroll-default-factor=10" }
! { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
! { dg-additional-options "-fdump-tree-original" }

subroutine test1
  implicit none
  integer :: i,j
  !$omp parallel do
  !$omp unroll partial(10)
  do i = 1,100
     !$omp parallel do
     do j = 1,100
     call dummy(i,j)
     end do
  end do

  !$omp taskloop
  !$omp unroll partial(10)
  do i = 1,100
     !$omp parallel do
     do j = 1,100
     call dummy(i,j)
     end do
  end do

end subroutine test1

! For the "parallel do", there should be 11 "omp for" loops, 10 for the inner loop, 1 for outer,
! for the "taskloop", there should be 10 "omp for" loops for the unrolled loop
! { dg-final { scan-tree-dump-times {#pragma omp for} 21 "omp_transform_loops" } }
! ... and two outer taskloops plus the one taskloops
! { dg-final { scan-tree-dump-times {#pragma omp taskloop} 3 "omp_transform_loops" } }


subroutine test2
  implicit none
  integer :: i,j
  do i = 1,100
  !$omp teams distribute
  !$omp unroll partial(10)
     do j = 1,100
     call dummy(i,j)
     end do
  end do

  do i = 1,100
  !$omp target teams distribute
  !$omp unroll partial(10)
     do j = 1,100
     call dummy(i,j)
     end do
  end do
end subroutine test2

! { dg-final { scan-tree-dump-times {#pragma omp distribute} 2 "omp_transform_loops" } }

! After unrolling there should be 10 copies of each loop body for each loop-nest
! { dg-final { scan-tree-dump-times "dummy" 40 "omp_transform_loops" } }

! { dg-final { scan-tree-dump-not {#pragma omp loop_transform} "original" } }
! { dg-final { scan-tree-dump-times {#pragma omp for nowait unroll_partial\(10\)} 1 "original" } }
! { dg-final { scan-tree-dump-times {#pragma omp distribute private\(j\) unroll_partial\(10\)} 2 "original" } }

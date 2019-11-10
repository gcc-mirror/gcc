! Exercise combined OpenACC directives.

! { dg-additional-options "-fdump-tree-gimple" }

subroutine test
  implicit none
  integer a(100), i, j, y, z


  ! PARALLEL
  
  !$acc parallel loop collapse (2)
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end parallel loop
  
  !$acc parallel loop gang
  do i = 1, 100
  end do
  !$acc end parallel loop

  !$acc parallel loop worker
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end parallel loop

  !$acc parallel loop vector
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end parallel loop

  !$acc parallel loop seq
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end parallel loop

  !$acc parallel loop auto
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end parallel loop

  !$acc parallel loop tile (2, 3)
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end parallel loop

  !$acc parallel loop independent
  do i = 1, 100
  end do
  !$acc end parallel loop

  !$acc parallel loop private (z)
  do i = 1, 100
     z = 0
  end do
  !$acc end parallel loop

  !$acc parallel loop reduction (+:y) copy (y)
  do i = 1, 100
  end do
  !$acc end parallel loop


  ! KERNELS

  !$acc kernels loop collapse (2)
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end kernels loop
  
  !$acc kernels loop gang
  do i = 1, 100
  end do
  !$acc end kernels loop

  !$acc kernels loop worker
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end kernels loop

  !$acc kernels loop vector
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end kernels loop

  !$acc kernels loop seq
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end kernels loop

  !$acc kernels loop auto
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end kernels loop

  !$acc kernels loop tile (2, 3)
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end kernels loop

  !$acc kernels loop independent
  do i = 1, 100
  end do
  !$acc end kernels loop

  !$acc kernels loop private (z)
  do i = 1, 100
     z = 0
  end do
  !$acc end kernels loop

  !$acc kernels loop reduction (+:y) copy (y)
  do i = 1, 100
  end do
  !$acc end kernels loop


  ! SERIAL
  
  !$acc serial loop collapse (2)
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end serial loop
  
  !$acc serial loop gang
  do i = 1, 100
  end do
  !$acc end serial loop

  !$acc serial loop worker
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end serial loop

  !$acc serial loop vector
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end serial loop

  !$acc serial loop seq
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end serial loop

  !$acc serial loop auto
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end serial loop

  !$acc serial loop tile (2, 3)
  do i = 1, 100
     do j = 1, 10
     end do
  end do
  !$acc end serial loop

  !$acc serial loop independent
  do i = 1, 100
  end do
  !$acc end serial loop

  !$acc serial loop private (z)
  do i = 1, 100
     z = 0
  end do
  !$acc end serial loop

  !$acc serial loop reduction (+:y) copy (y)
  do i = 1, 100
  end do
  !$acc end serial loop

end subroutine test


! { dg-final { scan-tree-dump-times "acc loop private.i. private.j. collapse.2." 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. gang" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. private.j. worker" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. private.j. vector" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. private.j. seq" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. private.j. auto" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. private.j. tile.2, 3" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. independent" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "private.z" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "omp target oacc_\[^ \]+ map.tofrom:y" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.i. reduction..:y." 3 "gimple" } }

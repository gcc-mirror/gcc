! { dg-do compile } 
! { dg-additional-options "-fdump-tree-gimple" }

! See also '../../c-c++-common/goacc/combined-reduction.c'.

subroutine foo ()
  implicit none
  integer :: p, k, s
  integer  :: a

  !$acc parallel loop reduction(+:a)
  do p = 1,5
  enddo
  !$acc end parallel loop

  !$acc kernels loop reduction(+:a)
  do k = 2,6
  enddo
  !$acc end kernels loop

  !$acc serial loop reduction(+:a)
  do s = 1,5
  enddo
  !$acc end serial loop

end subroutine

! { dg-final { scan-tree-dump-times "target oacc_parallel reduction..:a. map.tofrom.a." 1 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.p. reduction..:a." 1 "gimple" } }
! { dg-final { scan-tree-dump-times "target oacc_kernels map.force_tofrom:a .len: 4.." 1 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.k. reduction..:a." 1 "gimple" } }
! { dg-final { scan-tree-dump-times "target oacc_serial reduction..:a. map.tofrom.a." 1 "gimple" } }
! { dg-final { scan-tree-dump-times "acc loop private.s. reduction..:a." 1 "gimple" } }


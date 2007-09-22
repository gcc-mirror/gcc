! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-omplower" }
  !$omp  parallel
call bar
	!$omp end parallel
  !$omp 	 	p&
!$omp&arallel
call bar
!$omp e&
!$omp&ndparallel
!$omp  &
!$omp  &  &
!$omp pa&
!$omp rallel
call bar
!$omp end parallel
! Non-continuation !$omp must be followed by space, and my reading
! doesn't seem to allow tab there.  So such lines should be completely
! ignored.
!$omp	strange  !  { dg-warning "starts a commented line" }
end

! { dg-final { scan-tree-dump-times "pragma omp parallel" 3 "omplower" } }
! { dg-final { cleanup-tree-dump "omplower" } }

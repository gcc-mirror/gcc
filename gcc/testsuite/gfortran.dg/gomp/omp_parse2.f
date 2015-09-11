c { dg-do compile }
c { dg-options "-fopenmp -fdump-tree-omplower" }
!$omp  parallel
      call bar
c$omp	end parallel
C$omp 	 	p
*$omp+arallel
      call bar
!$omp e
!$omp+ndparallel
      end

! { dg-final { scan-tree-dump-times "pragma omp parallel" 2 "omplower" } }

! { dg-additional-options "-fdump-tree-original" }

      ! OMP PARALLEL gets parsed  and is properly handled
      ! But ACC& gives an error
      ! [Before: an error is printed but OMP parses 'parallel loop ...']
      subroutine one
        implicit none
        integer i
!$omp parallel
!$acc& loop independent  !  { dg-error "Wrong OpenMP continuation at .1.: expected !.OMP, got !.ACC" }
        do i = 1, 5
        end do
!$omp end parallel
      end

      ! [Before: Bogus 'Wrong OpenMP continuation' as it was read as continuation line!]
      subroutine two
!$omp parallel
!$acc loop independent  !  { dg-error "The !.ACC LOOP directive cannot be specified within a !.OMP PARALLEL region" }
       do i = 1, 5
       end do
!$omp end parallel
       end

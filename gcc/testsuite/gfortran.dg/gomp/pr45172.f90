! { dg-do compile }
! { dg-options "-O -fopenmp -fexceptions" }

  SUBROUTINE dbcsr_mult_m_e_e ( )
    LOGICAL, PARAMETER   :: use_combined_types = .FALSE.
    INTEGER, ALLOCATABLE, DIMENSION(:, :) ::  right_index_sr
    INTEGER, ALLOCATABLE, DIMENSION(:, :, :) ::  my_sizes
    INTEGER, ALLOCATABLE,  DIMENSION(:, :, :, :) :: all_sizes
    ALLOCATE (all_sizes(4, LBOUND(my_sizes,2):UBOUND(my_sizes,2), &
              LBOUND(my_sizes,3):UBOUND(my_sizes,3), 0:numnodes-1))
          IF (use_combined_types) THEN
             CALL mp_waitall (right_index_sr)
          ENDIF
          DO ki = 0, min_nimages-1
!$omp parallel default (none) &
!$omp reduction (+: flop_single, t_all, t_dgemm)
!$omp end parallel
          ENDDO
       checksum = dbcsr_checksum (product_matrix, error)
  END SUBROUTINE dbcsr_mult_m_e_e


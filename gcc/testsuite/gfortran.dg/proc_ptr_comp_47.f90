! { dg-do run }

MODULE distribution_types
  ABSTRACT INTERFACE
     FUNCTION dist_map_blk_to_proc_func ( row, col, nrow_tot, ncol_tot, proc_grid ) RESULT( reslt )
       INTEGER, INTENT( IN ) :: row, col, nrow_tot, ncol_tot
       INTEGER, DIMENSION( : ), INTENT( IN ) :: proc_grid
       INTEGER, DIMENSION( : ), ALLOCATABLE :: reslt
     END FUNCTION dist_map_blk_to_proc_func
  END INTERFACE
  TYPE, PUBLIC :: dist_type
     INTEGER, DIMENSION( : ), ALLOCATABLE :: task_coords
     PROCEDURE( dist_map_blk_to_proc_func ), NOPASS, POINTER :: map_blk_to_proc => NULL( )
  END TYPE dist_type
END MODULE distribution_types

MODULE sparse_matrix_types
  USE distribution_types,  ONLY : dist_type
  TYPE, PUBLIC :: sm_type
     TYPE( dist_type ) :: dist
  END TYPE sm_type
END MODULE sparse_matrix_types

PROGRAM comp_proc_ptr_test
  USE sparse_matrix_types,      ONLY : sm_type

 call  sm_multiply_a ()
CONTAINS
  SUBROUTINE sm_multiply_a (  )
    INTEGER :: n_push_tot, istat
    TYPE( sm_type ), DIMENSION( : ), ALLOCATABLE :: matrices_a, matrices_b
    n_push_tot =2
    ALLOCATE( matrices_a( n_push_tot + 1 ), matrices_b( n_push_tot + 1), STAT=istat )
    if (istat /= 0) STOP 1
    if (.not. allocated(matrices_a)) STOP 2
    if (.not. allocated(matrices_b)) STOP 3
    if (associated(matrices_a(1)%dist%map_blk_to_proc)) STOP 4
  END SUBROUTINE sm_multiply_a
END PROGRAM comp_proc_ptr_test


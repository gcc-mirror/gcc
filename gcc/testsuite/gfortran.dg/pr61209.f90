! { dg-do compile }
! { dg-options "-O -fbounds-check" }
MODULE array_types
  INTERFACE array_data
     MODULE PROCEDURE array_data_i1d
  END INTERFACE
  TYPE array_i1d_type
  END TYPE array_i1d_type
  TYPE array_i1d_obj
     TYPE(array_i1d_type), POINTER      :: low
  END TYPE array_i1d_obj
  TYPE dbcsr_type
     TYPE(array_i1d_obj)     :: local_rows
     LOGICAL                 :: local_indexing
  END TYPE dbcsr_type
  TYPE dbcsr_obj
     TYPE(dbcsr_type) :: m
  END TYPE dbcsr_obj
CONTAINS
  FUNCTION array_data_i1d(array) RESULT (DATA)
    TYPE(array_i1d_obj), INTENT(IN)          :: array
    INTEGER, DIMENSION(:), POINTER           :: DATA
    IF (ASSOCIATED (array%low)) THEN
    ENDIF
  END FUNCTION array_data_i1d
  SUBROUTINE dbcsr_make_index_list (matrix, thread_redist)
    TYPE(dbcsr_obj), INTENT(INOUT)           :: matrix
    LOGICAL, INTENT(IN)                      :: thread_redist
    INTEGER, ALLOCATABLE, DIMENSION(:, :)    :: blki
    INTEGER, DIMENSION(:), POINTER           :: local_rows, td
    INTEGER :: blk
    nthreads = 0
    IF (nthreads .GT. 0 .AND. thread_redist) THEN
       IF (matrix%m%local_indexing) THEN
          local_rows => array_data (matrix%m%local_rows)
       ENDIF
       CALL dbcsr_build_row_index_inplace (thr_c, nthreads)
       IF (matrix%m%local_indexing) THEN
          DO blk = 1, nblks
             IF (td(local_rows(blki(1, blk))) .EQ. ithread) THEN
             ENDIF
          ENDDO
       ENDIF
    ENDIF
  END SUBROUTINE dbcsr_make_index_list
END MODULE

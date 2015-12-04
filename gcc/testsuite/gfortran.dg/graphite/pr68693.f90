! { dg-options "-floop-nest-optimize -O2" }
MODULE dbcsr_index_operations
  INTERFACE dbcsr_build_row_index
  END INTERFACE
CONTAINS
  SUBROUTINE merge_index_arrays (new_row_i, new_col_i, new_blk_p, new_size,&
       old_row_i, old_col_i, old_blk_p, old_size,&
       add_ip, add_size, new_blk_d, old_blk_d,&
       added_size_offset, added_sizes, added_size, added_nblks, error)
    INTEGER, DIMENSION(new_size), &
      INTENT(OUT)                            :: new_blk_p, new_col_i, &
                                                new_row_i
    INTEGER, INTENT(IN)                      :: old_size
    INTEGER, DIMENSION(old_size), INTENT(IN) :: old_blk_p, old_col_i, &
                                                old_row_i
    INTEGER, DIMENSION(new_size), &
      INTENT(OUT), OPTIONAL                  :: new_blk_d
    INTEGER, DIMENSION(old_size), &
      INTENT(IN), OPTIONAL                   :: old_blk_d
    INTEGER, DIMENSION(:), INTENT(IN), &
      OPTIONAL                               :: added_sizes
    INTEGER, INTENT(OUT), OPTIONAL           :: added_size, added_nblks
    LOGICAL                                  :: multidata
    IF (add_size .GT. 0) THEN
       IF (old_size .EQ. 0) THEN
          IF (PRESENT (added_size)) added_size = SUM (added_sizes)
       ENDIF
    ELSE
       new_row_i(1:old_size) = old_row_i(1:old_size)
       new_col_i(1:old_size) = old_col_i(1:old_size)
       new_blk_p(1:old_size) = old_blk_p(1:old_size)
       IF (multidata) new_blk_d(1:old_size) = old_blk_d(1:old_size)
    ENDIF
  END SUBROUTINE merge_index_arrays
END MODULE dbcsr_index_operations

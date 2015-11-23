! { dg-options "-std=legacy -floop-nest-optimize -O2" }

MODULE dbcsr_mm_accdrv
  INTEGER, SAVE :: accdrv_binning_nbins = 4096
  INTEGER, SAVE :: accdrv_binning_binsize = 16
  INTEGER, PARAMETER, PUBLIC :: dbcsr_ps_width = 7
  CONTAINS
  SUBROUTINE stack_binning(params_in, params_out, stack_size)
    INTEGER, INTENT(IN)                      :: stack_size
    INTEGER, DIMENSION(dbcsr_ps_width, &
      stack_size), INTENT(OUT)               :: params_out
    INTEGER, DIMENSION(dbcsr_ps_width, &
      stack_size), INTENT(IN)                :: params_in
    INTEGER, DIMENSION(accdrv_binning_nbins) :: bin_top
    INTEGER, DIMENSION(dbcsr_ps_width)       :: val
    INTEGER, DIMENSION(dbcsr_ps_width, &
      accdrv_binning_binsize, &
      accdrv_binning_nbins)                  :: bin_arr
     DO i=1,stack_size
        val(:) = params_in(:,i)
        IF(bin_top(bin_id) > accdrv_binning_binsize) THEN
           params_out(:, top:top+bin_top(bin_id)-2) = bin_arr(:, 1:bin_top(bin_id)-1, bin_id)
        ENDIF
        bin_arr(:, bin_top(bin_id), bin_id) =  val(:)
        bin_top(bin_id) = bin_top(bin_id) + 1
     END DO
  END SUBROUTINE  stack_binning
END MODULE

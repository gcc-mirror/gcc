! { dg-do compile }
! { dg-require-effective-target lto }
! { dg-options "-flto" }
  SUBROUTINE dbcsr_test_read_args(narg, args)
    CHARACTER(len=*), DIMENSION(:), &
      INTENT(out)         :: args
    CHARACTER(len=80)     :: line
    DO
       args(narg) = line
    ENDDO
  END SUBROUTINE dbcsr_test_read_args

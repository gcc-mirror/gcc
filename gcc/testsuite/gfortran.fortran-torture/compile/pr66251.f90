SUBROUTINE dbcsr_data_convert (n)
  COMPLEX(KIND=4), DIMENSION(:), POINTER :: s_data_c
  COMPLEX(KIND=8), DIMENSION(:), POINTER :: t_data_z
  t_data_z(1:n) = CMPLX(s_data_c(1:n), KIND=8)
  CALL foo()
END SUBROUTINE dbcsr_data_convert


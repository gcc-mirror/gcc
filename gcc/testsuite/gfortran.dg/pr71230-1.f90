! { dg-do compile }
! { dg-options "-O2 -fbounds-check -ffast-math" }
  FUNCTION pw_integral_aa ( cc ) RESULT ( integral_value )
    COMPLEX(KIND=8), DIMENSION(:), POINTER :: cc
    integral_value = accurate_sum ( CONJG ( cc (:) ) * cc (:) )
  END FUNCTION pw_integral_aa

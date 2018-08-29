! PR tree-optimization/84117
! { dg-do compile }
! { dg-options "-O3 -ftrapv" }
  FUNCTION pw_integral_aa ( cc ) RESULT ( integral_value )
    COMPLEX(KIND=8), DIMENSION(:), POINTER :: cc
    integral_value = accurate_sum ( CONJG ( cc (:) ) * cc (:) )
  END FUNCTION pw_integral_aa

!PR fortran/32242
! { dg-do compile }
! { dg-options "-Wreturn-type" }

MODULE kahan_sum
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
  INTERFACE accurate_sum
    MODULE PROCEDURE kahan_sum_d1, kahan_sum_z1
  END INTERFACE accurate_sum
  TYPE pw_grid_type
     REAL (KIND=dp), DIMENSION ( : ), POINTER :: gsq
  END TYPE pw_grid_type
  TYPE pw_type
     REAL (KIND=dp), DIMENSION ( : ), POINTER :: cr
     COMPLEX (KIND=dp), DIMENSION ( : ), POINTER :: cc
     TYPE ( pw_grid_type ), POINTER :: pw_grid
  END TYPE pw_type
CONTAINS
 FUNCTION kahan_sum_d1(array,mask) RESULT(ks)         ! { dg-warning "not set" }
   REAL(KIND=dp), DIMENSION(:), INTENT(IN)  :: array
   LOGICAL, DIMENSION(:), INTENT(IN), &
     OPTIONAL                               :: mask
   REAL(KIND=dp)                            :: ks
 END FUNCTION kahan_sum_d1
  FUNCTION kahan_sum_z1(array,mask) RESULT(ks)        ! { dg-warning "not set" }
    COMPLEX(KIND=dp), DIMENSION(:), &
      INTENT(IN)                             :: array
    LOGICAL, DIMENSION(:), INTENT(IN), &
      OPTIONAL                               :: mask
    COMPLEX(KIND=dp)                         :: ks
  END FUNCTION kahan_sum_z1

FUNCTION pw_integral_a2b ( pw1, pw2 ) RESULT ( integral_value )
    TYPE(pw_type), INTENT(IN)                :: pw1, pw2
    REAL(KIND=dp)                            :: integral_value
     integral_value = accurate_sum ( REAL ( CONJG ( pw1 % cc ( : ) ) &
          *  pw2 % cc ( : ) ,KIND=dp) * pw1 % pw_grid % gsq ( : ) )
END FUNCTION pw_integral_a2b
END MODULE

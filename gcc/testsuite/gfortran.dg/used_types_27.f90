! { dg-do compile }
!
! PR fortran/56674
! PR fortran/58813
! PR fortran/59016
! PR fortran/59024
! The generic name 'atomic_kind_types' was keeping pointers to freed
! symbols, leading to random error-recovery ICEs.
!
! Original test case from Joost VandeVondele <Joost.VandeVondele@mat.ethz.ch>.

MODULE atomic_kind_types
  PUBLIC :: atomic_kind_type
CONTAINS
  INTEGER FUNCTION is_hydrogen(atomic_kind)
    TYPE(atomic_kind_type), pointer :: atomic_kind ! { dg-error "used before it is defined" }
  END FUNCTION
END MODULE

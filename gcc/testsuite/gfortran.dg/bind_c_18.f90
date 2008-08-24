! { dg-do compile }
! PR fortran/37201
!
! Before character arrays were allowed as bind(C) return value.
!
implicit none
  INTERFACE 
    FUNCTION my() BIND(C,name="my") RESULT(r) ! { dg-error "cannot be an array" }
      USE iso_c_binding
      CHARACTER(kind=C_CHAR) :: r(10)
    END FUNCTION
  END INTERFACE
  INTERFACE 
    FUNCTION two() BIND(C,name="two") RESULT(r) ! { dg-error "cannot be a character string" }
      USE iso_c_binding
      CHARACTER(kind=C_CHAR,len=2) :: r
    END FUNCTION
  END INTERFACE
END

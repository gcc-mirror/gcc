! { dg-do compile }
!
! PR fortran/38829
!
MODULE mExample
CONTAINS
      SUBROUTINE wrapper(y_c) bind(c)
      USE iso_c_binding
      type, bind(c) :: ty_c
        type(c_ptr) :: y_cptr
        Integer(c_int) ny
      end type
      type(ty_c) :: y_c
      END SUBROUTINE
END MODULE

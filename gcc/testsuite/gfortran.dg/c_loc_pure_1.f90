! { dg-do compile }
! { dg-options "-Wimplicit-interface" }
! PR 38220 - c_loc is pure and has an explicit interface
USE ISO_C_BINDING, ONLY: C_PTR, C_LOC
CONTAINS
  PURE SUBROUTINE F(x)
    INTEGER, INTENT(in), TARGET :: x
    TYPE(C_PTR) :: px
    px = C_LOC(x)
  END SUBROUTINE
END

! { dg-do compile }
! we didn't look at the right symbol when genrating code. This broke
! when array valued functions came into play.
module module_vec3d
  INTERFACE cross_product
     MODULE PROCEDURE cross_product3_R4_R8
  END INTERFACE
CONTAINS
  FUNCTION cross_product3_R4_R8 ()
    real(8) :: cross_product3_r4_r8(3)
    cross_product3_r4_r8 = 0
  END FUNCTION cross_product3_R4_R8
END MODULE module_vec3d

PROGRAM TEST
  use module_vec3d, only: cross_product
  real(8) :: c(3)
  c = cross_product()
END PROGRAM TEST

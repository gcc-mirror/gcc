! { dg-do compile }
! Verify that C_PTR is auto generated because it's needed by C_LOC.
! This tests that PR 32801 is fixed.
PROGRAM c_loc_prob
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC 
END PROGRAM c_loc_prob

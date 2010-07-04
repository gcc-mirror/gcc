! { dg-do compile }
! PR 38672 - this used to ICE.
MODULE globals
  TYPE :: type1
     integer :: x
  END TYPE type1
  TYPE (type1) :: pdm_bps
END module globals
BLOCK DATA
   use globals
END BLOCK DATA
! { dg-final { cleanup-modules "globals" } }

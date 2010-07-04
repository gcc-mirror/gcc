! { dg-do compile }
MODULE globals
  TYPE :: type1
     sequence
     integer :: x
  END TYPE type1
  TYPE (type1) :: pdm_bps
  common /co/ pdm_bps
END module globals
BLOCK DATA
   use globals
END BLOCK DATA

program main
  use globals
  common /co/ pdm_bps ! { dg-error "already in a COMMON block" }
end program main
! { dg-final { cleanup-modules "globals" } }

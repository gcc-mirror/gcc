! { dg-do compile }
! This checks that the fix for PR25730, which was a regression
! caused by the fix for PR19362.
!
! Contributed by Andrea Bedini <andrea.bedini@gmail.com>
!==============
MODULE testcase
  TYPE orbit_elem
     CHARACTER(4) :: coo
  END TYPE orbit_elem
END MODULE
MODULE tp_trace
  USE testcase
  TYPE(orbit_elem) :: tp_store
CONTAINS
  SUBROUTINE str_clan()
    USE testcase
    TYPE(orbit_elem) :: mtpcar
    mtpcar%coo='a'             !ICE was here
  END SUBROUTINE str_clan
END MODULE

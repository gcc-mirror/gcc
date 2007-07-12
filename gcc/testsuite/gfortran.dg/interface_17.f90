! { dg-do compile }
! Tests the fix for PR32727, which was a regression caused
! by the fix for PR32634
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE kinds
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND ( 14, 200 )
END MODULE kinds

MODULE util
  USE kinds,                           ONLY: dp
  INTERFACE sort
     MODULE PROCEDURE sort2
  END INTERFACE
CONTAINS
  SUBROUTINE sort2 ( )
  END SUBROUTINE sort2
END MODULE util

MODULE graphcon
  USE util,                            ONLY: sort
END MODULE graphcon
! { dg-final { cleanup-modules "kinds util graphcon" } }

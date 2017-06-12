! { dg-do compile }
! { dg-options "-finit-derived -finit-integer=12345678" }
!
! PR fortran/80668
!
! Test a regression where structure constructor expressions were created for
! POINTER components with -finit-derived.
!

MODULE pr80668
  IMPLICIT NONE
  TYPE :: dist_t
     INTEGER :: TYPE,nblks_loc,nblks
     INTEGER,DIMENSION(:),POINTER :: dist
  END TYPE dist_t

CONTAINS

  SUBROUTINE hfx_new()
    TYPE(dist_t)                             :: dist
    integer,pointer :: bob
    CALL release_dist(dist, bob)
  END SUBROUTINE hfx_new

  SUBROUTINE release_dist(dist,p)
    TYPE(dist_t)                             :: dist
    integer, pointer, intent(in) :: p
  END SUBROUTINE release_dist
END MODULE

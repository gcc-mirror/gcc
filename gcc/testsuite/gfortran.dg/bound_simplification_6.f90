! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/66100
! ICE on lbound simplification
!
! Original test case by Joost VandeVondele <Joost.VandeVondele@mat.ethz.ch>
! Reduced by Thomas Koenig <tkoenig@gcc.gnu.org>
!
MODULE qs_integrate_potential_low
  INTEGER, PARAMETER :: dp = 8
  TYPE cell_type
    REAL(KIND=8) :: h_inv(3,3)
  END TYPE
  TYPE(cell_type), POINTER                 :: cell
  REAL(KIND=dp), DIMENSION(3)              :: rp
  CONTAINS
    SUBROUTINE integrate_general_opt()
    REAL(KIND=dp) :: gp(3)
    INTEGER :: ng
    if (any(lbound(cell%h_inv) /= 1)) call abort
    if (any(ubound(cell%h_inv) /= 3)) call abort
    END SUBROUTINE integrate_general_opt
END MODULE qs_integrate_potential_low
! { dg-final { scan-tree-dump-not "bound" "original" } }
! { dg-final { scan-tree-dump-not "abort" "original" } }

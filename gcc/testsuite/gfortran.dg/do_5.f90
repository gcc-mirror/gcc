! { dg-do compile }
!
! PR fortran/54370
!
! The following program was ICEing at tree-check time
! "L()" was regarded as default-kind logical.
!
! Contributed by Kirill Chilikin
!
      MODULE M
      CONTAINS

      LOGICAL(C_BOOL) FUNCTION L() BIND(C)
      USE, INTRINSIC :: ISO_C_BINDING
      L = .FALSE.
      END FUNCTION

      LOGICAL(8) FUNCTION L2() BIND(C) ! { dg-warning "GNU Extension: LOGICAL result variable 'l2' at .1. with non-C_Bool kind in BIND.C. procedure 'l2'" }
      L2 = .FALSE._8
      END FUNCTION

      SUBROUTINE S()
      DO WHILE (L())
      ENDDO
      DO WHILE (L2())
      ENDDO
      END

      END

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

      LOGICAL(8) FUNCTION L2() BIND(C) ! { dg-warning "may not be a C interoperable kind but it is bind" }
      L2 = .FALSE._8
      END FUNCTION

      SUBROUTINE S()
      DO WHILE (L())
      ENDDO
      DO WHILE (L2())
      ENDDO
      END

      END

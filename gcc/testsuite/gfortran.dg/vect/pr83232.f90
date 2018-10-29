! { dg-do compile }
! { dg-require-effective-target vect_double }
! { dg-additional-options "-funroll-loops --param vect-max-peeling-for-alignment=0 -fdump-tree-slp-details" }

      SUBROUTINE MATERIAL_41_INTEGRATION ( STRESS,YLDC,EFPS,                   &
     &  DTnext,Dxx,Dyy,Dzz,Dxy,Dxz,Dyz,MatID,P1,P3 )
      REAL(KIND(0D0)), INTENT(INOUT) :: STRESS(6)
      REAL(KIND(0D0)), INTENT(IN)    :: DTnext
      REAL(KIND(0D0)), INTENT(IN)    :: Dxx,Dyy,Dzz,Dxy,Dxz,Dyz
      REAL(KIND(0D0)) :: Einc(6)
      REAL(KIND(0D0)) :: P1,P3

      Einc(1) = DTnext * Dxx ! (1)
      Einc(2) = DTnext * Dyy
      Einc(3) = DTnext * Dzz
      Einc(4) = DTnext * Dxy
      Einc(5) = DTnext * Dxz
      Einc(6) = DTnext * Dyz
      DO i = 1,6
        STRESS(i) = STRESS(i) + P3*Einc(i)
      ENDDO
      STRESS(1) = STRESS(1) + (DTnext * P1 * (Dxx+Dyy+Dzz)) ! (2)
      STRESS(2) = STRESS(2) + (DTnext * P1 * (Dxx+Dyy+Dzz))
      STRESS(3) = 0.0
      Einc(5) = 0.0  ! (3)
      Einc(6) = 0.0
      call foo (Einc)
      END SUBROUTINE

! We should vectorize (1), (2) and (3)
! { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "slp1" } }

! { dg-do run }
!*==CENTCM.spg  processed by SPAG 6.55Dc at 09:26 on 23 Sep 2005
      SUBROUTINE CENTCM
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      cm1 = 0.D0
      cm2 = 0.D0
      cm3 = 0.D0
      DO i = 1 , MOLsa
         cm1 = cm1 + X0(1,i)
         cm2 = cm2 + X0(2,i)
         cm3 = cm3 + X0(3,i)
      ENDDO
      cm1 = cm1/MOLsa
      cm2 = cm2/MOLsa
      cm3 = cm3/MOLsa
      IF ( (cm1.EQ.0.D0) .AND. (cm2.EQ.0.D0) .AND. (cm3.EQ.0.D0) )      &
     &     RETURN
      DO i = 1 , MOLsa
        X0(1,i) = X0(1,i) - cm1
        X0(2,i) = X0(2,i) - cm2
        X0(3,i) = X0(3,i) - cm3
        XIN(1,i) = XIN(1,i) - cm1
        XIN(2,i) = XIN(2,i) - cm2
        XIN(3,i) = XIN(3,i) - cm3
      ENDDO
      CONTINUE
      END
      PROGRAM test
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (NM=16384)
      PARAMETER (NG=100)
      PARAMETER (NH=100)
      PARAMETER (MU=20)
      PARAMETER (NL=1)
      PARAMETER (LL=10*NM)
      PARAMETER (KP=2001,KR=2001,KG=2001)
      COMMON /LCS   / X0(3,-2:NM) , X(3,-2:NM,5) , XIN(3,-2:NM)
      COMMON /MOLEC / LPBc(3) , MOLsp , MOLsa , NBX , NBY , NBZ , NPLa ,&
     &                LPBcsm
      MOLsa = 10
      X0 = 1.
      CALL CENTCM
      END

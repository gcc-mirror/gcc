! { dg-do compile }
! { dg-additional-options "-Ofast" }
      SUBROUTINE GAUBON(NV,PTS,PP)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER (MXSP=250)
      DIMENSION PTS(3,10),PP(3)
      COMMON /PCMPLY/ XE(MXSP),YE(MXSP),ZE(MXSP)
      DATA PI/3.141592653589793D+00/
      DATA ZERO/0.0D+00/
      DO I = 1, NV
      PP(1) = PP(1) + (PTS(1,I)-XE(NS))
      PP(2) = PP(2) + (PTS(2,I)-YE(NS))
      PP(3) = PP(3) + (PTS(3,I)-ZE(NS))
      ENDDO
      END

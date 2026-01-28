! { dg-do compile }
! { dg-additional-options "-Ofast" }
! { dg-additional-options "-mcpu=neoverse-v2" { target aarch64*-*-* } }"
!GCC$ builtin (expf) attributes simd (notinbranch)
      MODULE MODULE_CU_BMJ
      INTEGER:: JTB
      CONTAINS
      SUBROUTINE BMJDRVRQVCUTEN
REAL, DIMENSION(JTB) :: THEOLD,TOLDY2T
      DO KTH=1,KTHM
        TH=TH+DTH
        DENOM=TH
        IF (DENOM>EPS) THEN
           QS=EXP(0/DENOM)
        ELSE
           QS=0.
        ENDIF
        THEOLD(KTH)=EXP(ELOCP*QS)
      ENDDO
      CALL SPLINE
      END
      END

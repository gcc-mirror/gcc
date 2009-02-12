! { dg-do compile }
! { dg-options "-O2" }
      SUBROUTINE CASHES(E,HESS,FC,FA,NORB,NPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MXAO=2047)
      DIMENSION HESS(NPR),E(NORB,*),FC(*),FA(*)
      COMMON /IJPAIR/ IA(MXAO)
      COMMON /MCPAR / NFZC,NCORBS,NCI,NORBS,NORBX,NUM
      K=0
      DO 200 IU = 1,NORB - NCORBS
         I = IU + NCORBS
         II=IA(I)+I
         DO 100 J = 1,NCORBS
            IF (I.GT.NORBS) THEN
               HESS(K)=FC(II) + FA(II) - E(J,J)
            ELSE
               HESS(K)=FA(II) - E(I,I) - E(J,J) + FC(JJ) + FA(JJ)
            END IF
  100    CONTINUE
  200 CONTINUE
      RETURN
      END

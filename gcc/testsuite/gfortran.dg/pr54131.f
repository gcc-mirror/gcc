! { dg-do compile }
! { dg-options "-O2 -funroll-loops" }

      SUBROUTINE EFPGRD(IFCM,NAT,NVIB,NPUN,FCM,
     *                  DEN,GRD,ENG,DIP,NVST,NFTODO,LIST)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION DEN(*),GRD(*),ENG(*),DIP(*),LIST(*)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      COMMON /FGRAD / DEF(3,MXFGPT),DEFT(3,MXFRG),TORQ(3,MXFRG),
     *                ATORQ(3,MXFRG)
      IF(NVST.EQ.0) THEN
         CALL PUVIB(IFCM,IW,.FALSE.,NCOORD,IVIB,IATOM,ICOORD,
     *              ENG(IENG),GRD(IGRD),DIP(IDIP))
      END IF
      DO 290 IVIB=1,NVIB
               DO 220 IFRG=1,NFRG
                  DO 215  J=1,3
                     DEFT(J,IFRG)=GRD(INDX+J-1)
  215             CONTINUE
                  INDX=INDX+6
  220          CONTINUE
  290 CONTINUE
      END

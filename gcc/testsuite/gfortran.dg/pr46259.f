! PR tree-optimization/46259
! { dg-do compile }
! { dg-options "-O3" }
      SUBROUTINE RDSTFR(FRGMNT,IFRAG,PROVEC,FOCKMA,
     *                  MXBF,MXMO,MXMO2,NTMOF)
      PARAMETER (MXPT=100, MXFRG=50, MXFGPT=MXPT*MXFRG)
      CHARACTER*8 WORD,MNAME,PNAME,RNAME
      COMMON /FRGSTD/ CORD(3,MXPT),PCORD(3,MXPT),POLT(9,MXPT),
     *                INLPR(4*MXPT),IKFR(MXPT),IKLR(MXPT),
     *                MNAME(MXPT),PNAME(MXPT),RNAME(MXPT)
      DO 10 I=1,MXPT
         INLPR(4*(I-1)+1)=0
         INLPR(4*(I-1)+2)=0
         INLPR(4*(I-1)+3)=0
         INLPR(4*(I-1)+4)=0
         IKLR(I)=0
         RNAME(I)='        '
   10 CONTINUE
      END

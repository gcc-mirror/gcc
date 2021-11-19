! { dg-do compile }
! { dg-options "-O3 -std=legacy" }
      CALL UVSET(NX,NY,NZ,HVAR,ZET,NP,DZ,DKM,UM,VM,UG,VG,TM,DCDX, ! { dg-warning "Rank mismatch|Invalid procedure argument" }
     *ITY,ISH,NSMT,F)
         CALL DCTDX(NX,NY,NX1,NFILT,C(MLAG),DCDX(MLAG),HELP,HELPA,
     *   HELP,HELPA,FY,FYC,SAVEY)
      END
      SUBROUTINE PADEC(DKS,DKDS,HVAR,WM,WG,FN,NS,AN,BN,CN,IT)
      COMPLEX*16 WM(*),WG(*),FN(*),AN(*),BN(*),CN(*)
         BN(J)=F4+AS+GAMMA*F2
         CN(J)=F4-AS+GAMMA*F2
         FN(J)=(AS+F4-GAMMA*F2)*H2+(F4-AS-GAMMA*F2)*H0+
     *   H1*(F3-GAMMA/3.D0)+GAMMA*WG(J)-CONST
      END
      SUBROUTINE UVSET(NX,NY,NZ,HVAR,ZET,NP,DZ,DKM,UM,VM,UG,VG,TM,
     *WORK,ITY,IH,NSMT,F)
      DIMENSION HVAR(*),ZET(*),TM(*),DKM(*),UM(*),VM(*),UG(*),VG(*),
     *WORK(*)
      IF(IH.EQ.0) THEN
         CALL PADEC(DKM,VM,HVAR,WORK(LWM),WORK(LWG), ! { dg-warning "Rank mismatch" }
     *   WORK(LF),NZ,WORK(LA),WORK(LB),WORK(LC),ITY) ! { dg-warning "Type mismatch" }
      ENDIF
      END

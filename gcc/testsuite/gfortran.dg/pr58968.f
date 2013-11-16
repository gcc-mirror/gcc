C PR rtl-optimization/58968.f
C { dg-do compile { target powerpc*-*-*} }
C { dg-options "-mcpu=power7 -O3 -w -ffast-math  -funroll-loops" }
      SUBROUTINE MAKTABS(IW,SOME,LBOX1,LBOX2,LBOX3,NSPACE,NA,NB,
     *            LBST,X,
     *            NX,IAMA,IAMI,IBMA,IBMI,MNUM,IDIM,MSTA,IBO,
     *            IDSYM,ISYM1,NSYM,
     *            NACT,LWRK,KTAB,LGMUL,
     *            LCON,LCOA,LCOB,
     *            LANDET,LBNDET,NAST,NBST,LSYMA,LSYMB,LGCOM,
     *            MINI,MAXI,LSPA,LSPB,LDISB,
     *            LSAS,LSBS,LSAC,LSBC,
     *            ITGA,ITGB,IAST,IBST,NCI,NA1EX,NB1EX,FDIRCT)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL SOME
      DIMENSION LBOX1(NSPACE),LBOX2(NSPACE),LBOX3(NSPACE),LBST(NSPACE)
      DIMENSION X(NX)
      DIMENSION IAMA(NSPACE),IAMI(NSPACE),IBMA(NSPACE),IBMI(NSPACE)
      DIMENSION MNUM(NSPACE),IDIM(NSPACE),MSTA(NSPACE+1),IBO(NACT)
      DIMENSION LWRK(43),KTAB(NSYM),LGMUL(NSYM,NSYM)
      DIMENSION LCON(NA)
      DIMENSION LCOA(NSYM,ITGA),LCOB(NSYM,ITGB)
      DIMENSION LANDET(NSPACE,ITGA),LBNDET(NSPACE,ITGB)
      DIMENSION NAST(ITGA+1),NBST(ITGB+1)
      DIMENSION LSYMA(IAST),LSYMB(IBST)
      DIMENSION LGCOM(ITGB,ITGA)
      DIMENSION MINI(NSPACE),MAXI(NSPACE)
      DIMENSION LSPA(IAST),LSPB(IBST)
      DIMENSION LDISB(NSYM,ITGB,ITGA)
      DIMENSION LSAS(NSYM+1,ITGA),LSBS(NSYM+1,ITGB)
      DIMENSION LSAC(IAST),LSBC(IBST)
      LOGICAL FDIRCT
      LCOA = 0
      LCOB = 0
      ISTA1 = LBST(1)
      CALL RESETCO(LBOX1,NSPACE,NB,IBMA,IBMI,LBOX2)
      NAST(1) = 0
      NBST(1) = 0
      DO II=1,ITGA
         ITOT = 1
         DO JJ=1,NSPACE
            ITOT = ITOT * LANDET(JJ,II)
         ENDDO
         NAST(II+1) = NAST(II) + ITOT
      ENDDO
      DO II=1,ITGB
         ITOT = 1
         DO JJ=1,NSPACE
            ITOT = ITOT * LBNDET(JJ,II)
         ENDDO
         NBST(II+1) = NBST(II) + ITOT
      ENDDO
      ICOMP = 0
      CALL RESETCO(LBOX1,NSPACE,NA,IAMA,IAMI,LBOX3)
      NA1EX = 0
      NB1EX = 0
      CALL RESETCO(LBOX1,NSPACE,NB,IBMA,IBMI,LBOX3)
      DO IIB = 1,ITGB
         CALL RESETDE(LBOX1,NSPACE,NB,MSTA,LCON)
         DO KKB=NBST(IIB)+1,NBST(IIB+1)
            DO II=1,NSPACE
               LBOX2(II) = LBOX1(II)
            ENDDO
            IEBS = NB+1
            DO ISPB1=NSPACE,1,-1
               IOC1 = LBOX1(ISPB1)
               IEBE = IEBS - 1
               IEBS = IEBS - IOC1
               LBOX2(ISPB1) = LBOX2(ISPB1)-1
               DO IB1=IEBE,IEBS,-1
                  IO1 = LCON(IB1)
                  IGBE = IEBE - LBOX1(ISPB1)
                  DO ISPB2=ISPB1,NSPACE
                     IGBS = IGBE + 1
                     IGBE = IGBE + LBOX1(ISPB2)
                     LBOX2(ISPB2) = LBOX2(ISPB2) + 1
                     IGBA = MAX(IB1+1,IGBS)
                     DO IGAP=IGBA,IGBE+1
                        DO JJ=ISTA,IEND
                           NB1EX = NB1EX + 1
                        ENDDO
                        ISTA = LCON(IGAP)+1
                        IEND = LCON(IGAP+1)-1
                        IF (IGAP.EQ.IGBE) IEND=MSTA(ISPB2+1)-1
                     ENDDO
                     LBOX2(ISPB2) = LBOX2(ISPB2) - 1
                  ENDDO
               ENDDO
               LBOX2(ISPB1) = LBOX2(ISPB1) + 1
            ENDDO
            CALL MOVEUP2(LBOX1,NSPACE,NB,MSTA,LCON)
         ENDDO
         CALL PUSHCO(LBOX1,NSPACE,NB,IBMA,IBMI,LBOX3,IEND)
      ENDDO 
      RETURN
      END

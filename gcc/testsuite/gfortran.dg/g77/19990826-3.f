c { dg-do compile }
* Date: Thu, 19 Aug 1999 10:02:32 +0200
* From: Frederic Devernay <devernay@istar.fr>
* Organization: ISTAR
* X-Accept-Language: French, fr, en
* To: gcc-bugs@gcc.gnu.org
* Subject: g77 2.95 bug (Internal compiler error in `final_scan_insn')
* X-UIDL: 08443f5c374ffa382a05573281482f4f

* Here's a bug that happens only when I compile with -O (disappears with
* -O2)

* > g77 -v --save-temps -O -c  pcapop.f
* g77 version 2.95 19990728 (release) (from FSF-g77 version 0.5.25
* 19990728 (release))
* Reading specs from
* /usr/local/lib/gcc-lib/sparc-sun-solaris2.6/2.95/specs
* gcc version 2.95 19990728 (release)
*  /usr/local/lib/gcc-lib/sparc-sun-solaris2.6/2.95/f771 pcapop.f -quiet
* -dumpbase pcapop.f -O -version -fversion -o pcapop.s
* GNU F77 version 2.95 19990728 (release) (sparc-sun-solaris2.6) compiled
* by GNU C version 2.95 19990728 (release).
* GNU Fortran Front End version 0.5.25 19990728 (release)
* pcapop.f: In subroutine `pcapop':
* pcapop.f:291: Internal compiler error in `final_scan_insn', at
* final.c:2920
* Please submit a full bug report.
* See <URL:http://egcs.cygnus.com/faq.html#bugreport> for instructions.

C* PCAPOP
      SUBROUTINE PCAPOP(M1,M2,L1,L2,NMEM,N1,N2,IB,IBB,K3,TF,TS,TC,TTO)
      DIMENSION NVA(6),C(6),I(6)
C
C CALCUL DES PARAMETRES OPTIMAUX N1 N2 IB IBB
C
      TACC=.035
      TTRANS=.000004
      RAD=.000001
      RMI=.000001
      RMU=.0000015
      RDI=.000003
      RTE=.000003
      REQ=.000005
      VY1=3*RTE+RDI+8*REQ+3*(RAD+RMI+RMU)
      VY2=REQ+2*RAD
      AR2=M2*(2*(REQ+RMI)+3*RMU+M1*(2*RAD+REQ))
C     VARIATION DE L1,L2,
C
      TTOTOP=1.E+10
      N1CO=0
      N2CO=0
      IBCO=0
      IBBCO=0
      K3CO=0
      TESOP=0.
      TCOP=0.
      TFOP=0.
      INUN=7
      INDE=7
      IF(M1.LT.128)INUN=6
      IF(M1.LT.64)INUN=5
      IF(M1.LT.32)INUN=4
      IF(M2.LT.128)INDE=6
      IF(M2.LT.64)INDE=5
      IF(M2.LT.32)INDE=4
      DO 3 NUN =3,INUN
      DO 3 NDE=3,INDE
      N10=2**NUN
      N20=2**NDE
      NDIF=(N10-N20)
      NDIF=IABS(NDIF)
C POUR AVOIR CES RESULTATS FAIRE TOURNER LE PROGRAMME VEFFT1
      TCFFTU=0.
      IF(N10.EQ.128.AND.N20.EQ.128)TCFFTU=3.35
      IF(N10.EQ.64.AND.N20.EQ.64)TCFFTU=.70
      IF(N10.EQ.32.AND.N20.EQ.32)TCFFTU=.138
      IF(N10.EQ.16.AND.N20.EQ.16)TCFFTU=.0332
      IF(N10.EQ.8.AND.N20.EQ.8)TCFFTU=.00688
      IF(NDIF.EQ.64)TCFFTU=1.566
      IF(NDIF.EQ.96)TCFFTU=.709
      IF(NDIF.EQ.112)TCFFTU=.349
      IF(NDIF.EQ.120)TCFFTU=.160
      IF(NDIF.EQ.32)TCFFTU=.315
      IF(NDIF.EQ.48)TCFFTU=.154
      IF(NDIF.EQ.56)TCFFTU=.07
      IF(NDIF.EQ.16)TCFFTU=.067
      IF(NDIF.EQ.24)TCFFTU=.030
      IF(NDIF.EQ.8)TCFFTU=.016
      N30=N10-L1+1
      N40=N20-L2+1
      WW=VY1+N30*VY2
      NDOU=2*N10*N20
      IF((N10.LT.L1).OR.(N20.LT.L2)) GOTO 3
      NB=NMEM-NDOU-N20*(L1-1)
      NVC=2*N10*(N20-1)+M1
      IF(NB.LT.(NVC)) GOTO 3
      CALL VALENT(M1,N30,K1)
      CALL VALENT(M2,N40,K2)
      IS=K1/2
      IF((2*IS).NE.K1)K1=K1+1
      TFF=TCFFTU*K1*K2
      CALL VALENT(M2,N40,JOFI)
      IF(NB.GE.(K1*N20*N30+2*N20*(L1-1))) GOTO 4
      TIOOP=1.E+10
      IC=1
18    IB1=2*IC
      MAX=(NB-2*N20*(L1-1))/(N20*N30)
      IN=MAX/2
      IF(MAX.NE.2*IN) MAX=MAX-1
      K3=K1/IB1
      IBB1=K1-K3*IB1
      IOFI=M1/(IB1*N30)
      IRZ=0
      IF(IOFI*IB1*N30.EQ.M1) GOTO1234
      IRZ=1
      IOFI=IOFI+1
      IF(IBB1.EQ.0) GOTO 1234
      IF(M1.EQ.((IOFI-1)*IB1*N30+IBB1*N30)) GOTO 1233
      IRZ=2
      GOTO 1234
1233  IRZ=3
1234  IBX1=IBB1
      IF(IBX1.EQ.0)IBX1=IB1
      AR1=M2*(2*(RAD+RMI+RMU+REQ)+(M1-(IOFI-1)*IB1*N30)*2*(REQ+RAD))
     %+M2*(3*(REQ+RMU+RAD)+4*RMI+(M1-(IOFI-1)*IB1*N30)*(2*RAD+REQ)
     %+(IOFI-1)*IB1*N30*(2*RMI+REQ+RAD))
      AR5=(JOFI-1)*(N20-L2)*(M1-(IOFI-1)*IB1*N30)*(2*(RAD+RMU)+REQ)
     %*IOFI+(M2-(JOFI-1)*N40+L2-2)*(M1-(IOFI-1)*IB1*N30)*(2*(RAD+RMU
     %)+REQ)*IOFI
      WQ=((IOFI-1)*IB1+IBX1)*JOFI*WW
      AT1=N20*WQ
      AT2=N40*WQ
      QW=JOFI*(VY1+VY2*IB1*N30)
      AT3=IOFI*N40*QW
      AT4=(IOFI-1)*N40*QW
      AT5=JOFI*((IOFI-1)*N40*(IB1/IBX1)*(VY1+IBX1*N30*VY2)
     %+N40*((IB1/IBX1)*(IOFI-1)+1)*(VY1+IBX1*N30*VY2))
      AT6=JOFI*((IOFI-1)*N40*(IB1/2)*(VY1+2*N30*VY2)+N40*(
     %IB1*(IOFI-1)/2+IBX1/2)*(VY1+2*N30*VY2))
      T1=JOFI*N20*(L1-1)*REQ
      T2=M1*(L2-1)*REQ
      T3=JOFI*N20*IBX1*N30*(RAD+REQ)
      T4=JOFI*((IOFI-1)*IB1*N30*N20*(2*RMI+REQ)+IBX1*N30*N20*(2*RMI+R
     %EQ))
      T5=JOFI*((IOFI-1)*IB1/2+IBX1/2)*N20*N30*(2*RAD+REQ)
      T6=2*JOFI*(((IOFI-1)*IB1+IBX1)*N20)*((5*(RMI+RMU)+4*RAD
     %)+(L1-1)*(2*RAD+REQ)+N30*(2*RAD+REQ))
      T7=JOFI*2*((IOFI-1)*IB1+IBX1)*(L1-1)*(2*RAD+REQ)
      T8=JOFI*N10*N20*((IOFI-1)*IB1/2+IBX1/2)*(3*REQ+9*RAD+4*RMU+RMI)
      T9=N10*N20*JOFI*((IOFI-1)*IB1/2+IBX1/2)*(REQ+RMI)+M1*M2*(REQ+R
     %DI+2*RAD)
      T10=JOFI*((IOFI-1)*IB1/2+IBX1/2)*2*(3*RMU+2*(RMI+RAD)+N40*(3*RMI
     %+4*RMU+3*(RAD+REQ)+N30*(2*RAD+REQ)))
      POI=JOFI
      IF(POI.LE.2)POI=2
      TNRAN=(N40+(POI-2)*N20+(M2-(JOFI-1)*N40+L2-1))*(RMI+RMU+RAD
     %+REQ+N30*(2*RAD+2*REQ)*(IB1*(IOFI-1)+IBX1))
      IF(TNRAN.LT.0.)TNRAN=0.
      TCPU=T1+T2+T3+T4+T5+T6+T7+T8+T9+T10+TNRAN
      NVA(1)=N40
      NVA(2)=N40
      NVA(3)=N20
      NVA(4)=N20
      NVA(5)=M2-(JOFI-1)*N40
      NVA(6)=NVA(5)
      C(1)=FLOAT(IB1*N30)/FLOAT(M1)
      C(2)=FLOAT(M1-(IOFI-1)*IB1*N30)/FLOAT(M1)
      C(3)=C(1)
      C(4)=C(2)
      C(5)=C(1)
      C(6)=C(2)
      K=1
      P1=FLOAT(NB)/FLOAT(M1)
10    IP1=P1
      I(K)=1
      IF(IP1.GE.NVA(K)) GOTO 7
      P2=P1
      IP2=P2
8     P2=P2-FLOAT(IP2)*C(K)
      IP2=P2
      IF(IP2.EQ.0) GOTO 3
      IP1=IP1+IP2
      I(K)=I(K)+1
      IF(IP1.GE.NVA(K))GOTO 7
      GOTO 8
7     IF(K.EQ.6) GOTO 11
      K=K+1
      GOTO 10
11    IP1=0
      IP2=0
      IP3=0
      POFI=JOFI
      IF(POFI.LE.2)POFI=2
      TIOL=(I(2)+(IOFI-1)*I(1)+(POFI-2)*(IOFI-1)*I(3)+(POFI-
     %2)*I(4)+(IOFI-1)*I(5)+I(6))*TACC+(IOFI*M1*N40+(POFI-2)*IOFI*
     %M1*N20+(M2-(JOFI-1)*N40+L2-1)*M1*IOFI)*TTRANS
      IF(IBB1.EQ.0) GOTO 33
      IF(IB1.EQ.IBB1) GOTO 33
      IF(IBB1.EQ.2)GOTO 34
      IP3=1
      INL=NMEM/((IOFI-1)*IB1*N30+IBB1*N30)
55    IF(INL.GT.N40)INL=N40
      GOTO 35
33    IF(IB1.GT.2) GOTO 36
      IF((M1-(IOFI-1)*IB1*N30).GE.N30) GOTO 36
34    IP1=1
      INL=NMEM/(2*M1-(IOFI-1)*IB1*N30)
      GOTO 55
36    IP2=1
      INL=NMEM/(IOFI*IB1*N30)
      IF(INL.GT.N40)INL=N40
35    CALL VALENT(N40,INL,KN1)
      CALL VALENT(M2-(JOFI-1)*N40,INL,KN2)
      CALL VALENT(INL*IBB1,IB1,KN3)
      CALL VALENT((N40-(KN1-1)*INL)*IBB1,IB1,KN4)
      IF((IP1+IP2+IP3).NE.1) CALL ERMESF(14)
      TIO1=0.
      IF(IP3.EQ.1)TIO1=N30*M2*TTRANS*(IB1*(IOFI-1)+IBB1)
      IF(IP1.EQ.1)TIO1=M1*M2*TTRANS
      IF(IP2.EQ.1) TIO1=(IB1*N30*M2*IOFI*TTRANS)
      TTIO=2.*TIO1+(KN1*IOFI*(JOFI-1)+KN2*IOFI+(KN1-1)*(
     %JOFI-1)+IOFI*(JOFI-1)+KN2-1.+IOFI+(KN1*(JOFI-1)+KN2))*TACC
     %+M1*M2*TTRANS+TIOL
      IF((IP1.EQ.1).AND.(IRZ.EQ.0))TCPU=TCPU+AT1+AT2+AT3
      IF((IP1.EQ.1).AND.(IRZ.NE.0))TCPU=TCPU+AT1+AT2+AT4+AR1
      IF((IP2.EQ.1).AND.(IRZ.EQ.0))TCPU=TCPU+AT1+AT2+AT3
      IF((IP2.EQ.1).AND.(IRZ.NE.0))TCPU=TCPU+AT1+AT2+AT3+AR2
      IFOIS=IB1/IBX1
      IF((IP3.EQ.1).AND.(IFOIS*IBX1.EQ.IB1))TCPU=TCPU+AT1+AT2+AT5+AR2
      IF((IP3.EQ.1).AND.(IFOIS*IBX1.NE.IB1))TCPU=TCPU+AT1+AT2+AT6+AR2
      IF((IP1.EQ.1).AND.(IRZ.EQ.1))TCPU=TCPU+AR5
      IF((IP1.EQ.1).AND.(IRZ.EQ.2))TCPU=TCPU+AR5
      TTIOG=TTIO+TCPU
      IF(TTIOG.LE.0.) GOTO 99
      IF(TTIOG.GE.TIOOP) GOTO 99
      IBOP=IB1
      IBBOP=IBB1
      K3OP=K3
      TIOOP=TTIOG
      TIOOP1=TTIO
      TIOOP2=TCPU
99    IF(IB1.GE.MAX)GOTO17
      IC=IC+1
      GOTO 18
4     T1=JOFI*N20*(L1-1)*REQ
      T2=M1*(L2-1)*REQ
      T3=JOFI*N20*N30*(RAD+REQ)*K1
      T4=JOFI*(K1*N30*N20*(2*RMI+REQ))
      T5=JOFI*N20*N30*(2*RAD+REQ)*K1/2
      T6=2*JOFI*(K1*N20)*((5*RMI+RMU)+4*RAD+(L1-1)*(2*RAD+REQ)+N30*2*
     %RAD+REQ)
      T7=JOFI*2*K1*(L1-1)*(2*RAD+REQ)
      T9=JOFI*N10*N20*K1*(REQ+RMI)/2+M1*M2*(REQ+RDI+2*RAD)
      T8=JOFI*N10*N20*K1*(3*REQ+9*RAD+4*RMU+RMI)/2
      T10=JOFI*K1*(3*RMU+2*(RMI+RAD)+N40*(3*RMI
     %+4*RMU+3*(RAD+REQ)+N30*(2*RAD+REQ)))
      PIO=JOFI
      IF(PIO.LE.2)PIO=2
      TNR=(N40+(PIO-2)*N20+(M2-(JOFI-1)*N40+L2-1))*(RMU+RMI+RAD+REQ+
     %N30*(2*RAD+2*REQ)*K1)
      IF(TNR.LE.0.)TNR=0.
      BT1=JOFI*N20*WW*K1
      BT2=JOFI*N40*WW*K1
      BT3=JOFI*N40*(VY1+K1*N30*VY2)
      BR1=M2*(2*(RAD+RMI+RMU+REQ)+(M1*2*(REQ+RAD)))+M2*(3*(
     $REQ+RAD+RMU)+4*(RMI)+M1*(2*(RAD)+REQ))
      BR2=M2*(2*(REQ+RMI)+3*RMU+M1*(2*RAD+REQ))
      TCPU=T1+T2+T3+T4+T5+T6+T7+T8+T9+T10
      TCPU=TCPU+TNR+BT1+BT2
      LIOF=M1/(N30)
      IRZ=0
      IF(LIOF*N30.EQ.M1) GOTO 2344
      IRZ=1
2344  IF(IRZ.EQ.0)TCPU=TCPU+BT3
      IF(IRZ.NE.0)TCPU=TCPU+BT3+BR2
      TIOOP=2.*FLOAT(M1)*FLOAT(M2)*TTRANS+2.*FLOAT(K2)*TACC+TCPU
      IBOP=1
      IBBOP=0
      K3OP=1
      TIOOP2=TCPU
      TIOOP1=TIOOP-TCPU
17    TTOT=TIOOP+TFF
      IF(TTOT.LE.0.) GOTO 3
      IF(TTOT.GE.TTOTOP)GOTO3
      N1CO=N10
      N2CO=N20
      IBCO=IBOP
      IBBCO=IBBOP
      K3CO=K3OP
      TTOTOP=TTOT
      TESOP=TIOOP1
      TCOP=TIOOP2
      TFOP=TFF
3     CONTINUE
 
      N1=N1CO
      N2=N2CO
      TTO=TTOTOP
      IB=IBCO
      IBB=IBBCO
      K3=K3CO
      TC=TCOP
      TS=TESOP
      TF=TFOP
      TT=TCOP+TFOP
      TWER=TTO-TT
      IF(N1.EQ.0.OR.N2.EQ.0) CALL OUTSTR(0,'PAS DE PLACE MEMOIRE SUFFISA
     $NTE POUR UNE MISE EN OEUVRE PAR BLOCS$')
      IF(IB.NE.1)RETURN
      IHJ=(M1/(N1-L1+1))
      IF(IHJ*(N1-L1+1).NE.M1)IHJ=IHJ+1
      IHJ1=IHJ/2
      IF(IHJ1*2.NE.IHJ)GOTO7778
      IB=IHJ
      IBB=0
      RETURN
7778  IB=IHJ+1
      IBB=0
      RETURN
      END

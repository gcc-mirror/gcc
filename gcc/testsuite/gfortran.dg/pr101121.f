! { dg-do compile }
! { dg-options "-Ofast -std=legacy" }
! { dg-additional-options "-march=haswell" { target x86_64-*-* i?86-*-* } }
      COMMON /JMSG80/ PI4,PIF,P120,R12,P340,R34,FCS(4,3),E34MAX,
     7                IJSAME,KLSAME,IKSMJL
      DIMENSION P1(3),FQ(0:5),F1(0:4),F2(0:4),WS(8),WP(8)
      DIMENSION VEA(12),VES(9),WES(6)
      DIMENSION T(0:20),U(0:20)
      DIMENSION T3R(6,3,3,3),T9B(0:20,3,3,3)
      DIMENSION F5X(0:12,3,3,3),F6X(0: 6,3,3,3,3)
      DIMENSION A31(0:3,0:3),C31(2,0:3),A32(0:3,0:3),C32(2,0:3)
      DIMENSION A41(0:3,0:3),C41(2,0:3),A42(0:3,0:3),C42(2,0:3)
      DIMENSION A33(16),FIJ00(2),A43(16),FI0K0(2)
      DIMENSION SEJJK0(  3),A54(16,  3),C54(2,  3)
      DIMENSION A56(0:22,3,0:3),C56(2,0:3)
      DIMENSION A60(0:3,0:3),C60(2,0:3),A61(0:3,0:3),C61(2,0:3)
      DIMENSION A62(16),FI00L(2),A63(16),F0J0L(2)
      DIMENSION A64(0:3,0:3),C64(2,  3),A65(0:3,0:3),C65(2,  3)
      DIMENSION A69(0:3,  3),C69(2,0:3),A70(0:3,  3),C70(2,0:3)
      DIMENSION A71(18,  3),C71(2,  3)
      DIMENSION A72(18,  3),C72(2,  3)
      DIMENSION A73(18,0:3),C73(2,0:3)
      DIMENSION SE0LKL(  3),A75(16,3),C75(2,0:3)
      DIMENSION SE0JLL(  3),A76(16,3),C76(2,0:3)
      DIMENSION A77(0:25,3,0:3),C77(2,0:3),A78(0:31,3,0:3),C78(2,0:3)
      DIMENSION A79(0:31,3,0:3),C79(2,0:3)
      DIMENSION A80(0: 2,2),A81(0:24,3),A82(0:31,2),A83(0:22,2)
      DIMENSION A84(0:13,2),A85(0:13,2),A86(0: 6)
      DIMENSION S4(0:14),Q4(0:4),FIJKL(2)
                  IF(XVA.LT.CUG) THEN
                  ENDIF
                     F1(M)= FQ0*TMP
                     F2(M)= FQ0*TMP
                  XX1=-X12*X43
      IF(JI.EQ.1) THEN
            DO 255 J=1,3
  255    CONTINUE
         DO 268 K=1,3
            SEJJK00= E0+E(2,2,K,0)+E(3,3,K,0)
            A54( 5,K)= A540
  268    CONTINUE
  297       F5X(3+M,I,I,I)=-R3(M,I,I,I)
            DO 299 J=1,3
                     F5X(3+M,I,I,J)=-R3(M,J,I,I)
  299    CONTINUE
         DO 300 L=0,M56
            DO 300 M=1,3
  300    A56(N,M,L)= ZER
               A60(2,L)= A600+P34(I,3)*E(I,0,0,L)
               A61(0,L)= A610+D1I     *E(L,0,0,I)
               A61(1,L)= A610+P12(I,3)*E(L,0,0,I)
         SEL00L= E(1,0,0,1)+E(2,0,0,2)+E(3,0,0,3)
               IF(I.NE.J) THEN
                  K=6-I-J
                  F6X(0,J,I,I,I)= ZER
                  F6X(0,I,J,I,I)= ZER
                  F6X(0,I,I,J,I)= ZER
                  F6X(0,I,I,I,J)= ZER
                     F6X(M,I,I,K,J)= R2(M,K,J)
               ENDIF
  391       A82( M,N)= ZER
  392       A83( M,N)= ZER
               A84(M,N)= ZER
               A85(M,N)= ZER
  397    A86( M)= ZER
         DO 399 K=1,3
            DO 399 J=1,3
                  DO 398 M=1,6
                     T9B(M+ 2,I,J,K)= T3R0
                     T9B(M+ 8,I,J,K)= T1R(M,I,J,K)
                     T9B(M+14,I,J,K)= T3R0
  398             CONTINUE
  399    CONTINUE
  417                A77( M,3,K)= A770+F5X0*GEIJKL
  445                A81( M,3) = A81( M,3)+T( M)*TMP
                     IF(K.EQ.L)A81( 5,3)=A81( 5,3)+TMP
                     IF(I.EQ.J) THEN
                        DO 447 M=6,11
  447                   A81( M,3) = A81( M,3)+T( M)*GEIJKL
                     ENDIF
      ENDIF
      IF(LK.EQ.1) THEN
         IF(JTYPE.NE.4) THEN
            DO 510 J=0,3
               A31(3,J)= A310+ A310*Y02
               A32(3,J)= A320+ A320*Y02
  510       CONTINUE
            A33( 6)=-AEIJ00*Y1Y+T01
            A33( 7)= A330-0*Y01+T01
            A33( 8)= A330- A330*Y01
            A33(15)= A330+0*Y02
            A33(16)= A330+ A330*Y02
         ENDIF
            A84(12,N)= A84( 7,N)+ A84( 8,N)*Y02
            A84(13,N)= A84( 9,N)
         A85(10,2)= A85(10,2)- A85(10,1)+ A850
         A85(11,2)= A85(11,2)- A85(11,1)+ A850
         A85(12,2)= A85(12,2)- A85(12,1)+ A850
         A85(13,2)= A85(13,2)- A85(13,1)
         Q4(0)= S4( 0)+(S4( 1)+(S4( 2)+(S4( 3)+S4( 4)*Y02)*Y02)*Y02)*Y02
         Q4(1)= S4( 5)+(S4( 6)+(S4( 7)+ S4( 8)*Y02)*Y02)*Y02
         Q4(2)= S4( 9)+(S4(10)+ S4(11)*Y02)*Y02
         Q4(3)= S4(12)+ S4(13)*Y02
         Q4(4)= S4(14)
      ENDIF
      IF(JTYPE.NE.4) THEN
      ENDIF
         C42(1,M)= T0*F10-T0*F10
         C42(2,M)= T0*F20-T0*F20
      T(1)= T01+(A43( 4)- A43( 5)*Y04)*Y04
      F0J0L(2)= T(0)*F20-T0*F20+T(2)*F20
      DO 660 N=1,3
         T(0)= A64(3,N)- A64(0,N)- A64(1,N)
         T(1)= A640- A640*Y04
         C64(1,N)= T0*F10-T0*F10
         C64(2,N)= T0*F20-T0*F2(1)
         C65(1,N)= T0*F10-T0*F10
         C65(2,N)= T0*F20-T0*F2(1)
         C70(2,N)= T0*F20-T0*F20
         T(2)=(A71(17,N)-(A71(18,N)- A71(16,N)*Y04)*Y04)*XX1
         C71(1,N)= T0*F10-T0*F10+T0*F10
         C71(2,N)= T0*F20-T0*F20+T0*F20
         T(1)=(A720+ A720- A720-T0)*XX1
         C72(1,N)= T0*F10-T0*F10+T0*F10
         C75(1,N)= T(0)*F10-T0*F1(1)+T(2)*F1(2)
         C75(2,N)= T(0)*F20-T0*F2(1)+T(2)*F2(2)
         T01 = A76( 6,N)*XX1
         T(1)=(T01- A760-(A760- A76( 7,N)- A76( 8,N)
     2                       -(A760+ A76( 3,N))*Y04)*Y04)*XX1
  660 CONTINUE
         C73(2,M)= T0*F20+T0*F20+T(2)*F20
     2       +(A77(23,1,M)+ A77(24,1,M)*Y04)*Y03
         T(2)=(A77(21,2,M)+(A77(22,2,M)+ A77(23,2,M)*Y04)*Y04)*XX1
     2       -(A77(24,2,M)-(A77(25,2,M)+ A77(20,2,M)*Y04)*Y04)*Y03
         T(3)=(A77(21,3,M)+(A77(22,3,M)+(A77(23,3,M)
     2                                 + A77(24,3,M)*Y04)*Y04)*Y04)*XX1
         C77(1,M)= T0*F10-T0*F10-T0*F10+T0*F10
         C77(2,M)= T(0)*F20-T(1)*F20-T(2)*F20+T(3)*F20
         T(0)=(A780+ A78(24,1,M))*Y3Y+ A780*XX1
         T(1)=(A78(23,1,M)+(A78(21,1,M)+A78(22,1,M)*Y04)*Y04)*XX1
     2       +(A78(25,1,M)+ A78(26,1,M)*Y04)*Y3Y- A78(27,1,M)*XX2
         T(2)=(A78(21,2,M)+(A78(22,2,M)+ A78(28,2,M)*Y04)*Y04)*XX2
     2       +(A78(23,2,M)-(A78(24,2,M)+ A78(25,2,M)*Y04)*Y04)*XX1
     3       -(A78(29,2,M)-(A78(30,2,M)+ A78(31,2,M)*Y04)*Y04)*Y41
         T(3)=(A78(21,3,M)+(A78(22,3,M)+(A78(23,3,M)
     2                                 + A78(24,3,M)*Y04)*Y04)*Y04)*XX2
         C78(1,M)= T0*F10-T0*F1(1)-T(2)*F1(2)+T(3)*F1(3)
         C78(2,M)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)
         T(0)=-A79(24,1,M)*Y3Y+ A79(30,1,M)*H43
         T(1)=(A79(21,1,M)-(A79(23,1,M)+ A79(22,1,M)*Y04)*Y04)*XX1
     2       +(A79(25,1,M)+ A79(26,1,M)*Y04)*Y3Y- A79(29,1,M)*XX2
         T(2)=(A79(21,2,M)+(A79(22,2,M)- A79(28,2,M)*Y04)*Y04)*XX2
     2       +(A79(23,2,M)-(A79(24,2,M)+ A79(25,2,M)*Y04)*Y04)*XX1
     3       -(A79(29,2,M)-(A79(30,2,M)+ A79(31,2,M)*Y04)*Y04)*Y41
         T(3)=(A79(21,3,M)+(A79(22,3,M)+(A79(23,3,M)
     2                                 + A79(24,3,M)*Y04)*Y04)*Y04)*XX2
         C79(1,M)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)
         C79(2,M)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)
      T(0)= A80( 2,1)*Y3Y+ A80( 2,2)*H43
      T(1)=(A81(16,1)+(A81(14,1)+A81(15,1)*Y04)*Y04)*XX1
     2    +(A81(18,1)+ A81(19,1)*Y04)*Y3Y- A81(20,1)*XX2
      T(2)=(A82(21,1)+(A82(22,1)+ A82(28,1)*Y04)*Y04)*XX2
     2    +(A82(23,1)-(A82(24,1)+ A82(25,1)*Y04)*Y04)*XX1
     3    +(A83(15,2)+(A83(16,2)+ A82(31,1)*Y04)*Y04)*Y41
     4    -(A83(17,2)-(A83(18,2)- A83(19,2)*Y04)*Y04)*Y3Y
      T(3)=(A84(10,1)+(A84(11,1)+(A84(12,1)+A84(13,1)*Y04)*Y04)*Y04)*XX2
     2    +(A85(10,1)+(A85(10,2)+(A85(11,2)+(A85(12,2)
     3                          + A85(13,2)*Y04)*Y04)*Y04)*Y04)*XX1
      T(4)=(Q4(0)+(Q4(1)+(Q4(2)+(Q4(3)+Q4(4)*Y04)*Y04)*Y04)*Y04)*XX2
      FIJKL(1)= T(0)*F1(0)-T(1)*F1(1)-T(2)*F1(2)+T(3)*F1(3)+T(4)*F1(4)
      FIJKL(2)= T(0)*F2(0)-T(1)*F2(1)-T(2)*F2(2)+T(3)*F2(3)+T(4)*F2(4)
                  DO 800 ICP=1,2
                     VE0= VE0+C61(ICP,0)*WP(1)+FI00L(ICP)*WP(2)
     2                       +F0J0L(ICP)*WP(3)+C77(ICP,0)*WP(4)
     3                       +C73(ICP,0)*WP(5)+C78(ICP,0)*WP(6)
     4                       +C79(ICP,0)*WP(7)+FIJKL(ICP)*WP(8)
                     IF(ICP.EQ.1) THEN
                        DO 720 M=1,3
                           VES(  M)= VES(  M)+VEA(  M)
                           VES(3+M)= VES(3+M)+VEA(3+M)
                           T01 = VEA(6+M)
                           T02 = VEA(9+M)
                           WES(  M)=-T01+(T01+T02)*Y03
  720                   CONTINUE
                        FE1= X24*VE0
                        DO 730 M=1,3
                           T01 = VEA(  M)+VEA(3+M)
                           T02 = VEA(6+M)+VEA(9+M)
                           WES(3+M)=-T01+(T01+T02)*Z02
  730                   CONTINUE
                     ENDIF
  800             CONTINUE
                     WES(  M)= WES(  M)+P34(M,3)*FE0
                     WES(3+M)= WES(3+M)-P1(M)*FE1
                     VES(6+M)= VES(6+M)+WES(3+M)
                     FCS(3,M)= FCS(3,M)-WES(3+M)*Y03+WES(  M)
            DO 925 M=1,3
               T01= VES(  M)
               T02= VES(3+M)+VES(6+M)
               T01=-T01+(T01+T02)*Y01+P12(M,3)*TMP
               FCS(2,M)= FCS(2,M)-T01+VES(6+M)
  925       CONTINUE
      END

! PR rtl-optimization/37243
! { dg-do run }
! { dg-add-options ieee }
! Check if register allocator handles IR flattening correctly.
      SUBROUTINE SCHMD(V,M,N,LDV)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL GOPARR,DSKWRK,MASWRK
      DIMENSION V(LDV,N)
      COMMON /IOFILE/ IR,IW,IP,IS,IPK,IDAF,NAV,IODA(400)
      COMMON /PAR   / ME,MASTER,NPROC,IBTYP,IPTIM,GOPARR,DSKWRK,MASWRK
      PARAMETER (ZERO=0.0D+00, ONE=1.0D+00, TOL=1.0D-10)
      IF (M .EQ. 0) GO TO 180
      DO 160 I = 1,M
      DUMI = ZERO
      DO 100 K = 1,N
  100 DUMI = DUMI+V(K,I)*V(K,I) ! { dg-warning "Obsolescent feature: DO termination statement which is not END DO or CONTINUE" }
      DUMI = ONE/ SQRT(DUMI)
      DO 120 K = 1,N
  120 V(K,I) = V(K,I)*DUMI ! { dg-warning "Obsolescent feature: DO termination statement which is not END DO or CONTINUE" }
      IF (I .EQ. M) GO TO 160
      I1 = I+1
      DO 140 J = I1,M
      DUM = -DDOT(N,V(1,J),1,V(1,I),1)
      CALL DAXPY(N,DUM,V(1,I),1,V(1,J),1)
  140 CONTINUE
  160 CONTINUE
      IF (M .EQ. N) RETURN
  180 CONTINUE
      I = M
      J = 0
  200 I0 = I
      I = I+1
      IF (I .GT. N) RETURN
  220 J = J+1
      IF (J .GT. N) GO TO 320
      DO 240 K = 1,N
  240 V(K,I) = ZERO ! { dg-warning "Obsolescent feature: DO termination statement which is not END DO or CONTINUE" }
      CALL DAXPY(N,DUM,V(1,I),1,V(1,I),1)
  260 CONTINUE
      DUMI = ZERO
      DO 280 K = 1,N
  280 DUMI = DUMI+V(K,I)*V(K,I) ! { dg-warning "Obsolescent feature: DO termination statement which is not END DO or CONTINUE" }
      IF ( ABS(DUMI) .LT. TOL) GO TO 220
      DO 300 K = 1,N
  300 V(K,I) = V(K,I)*DUMI ! { dg-warning "Obsolescent feature: DO termination statement which is not END DO or CONTINUE" }
      GO TO 200
  320 END
      program main
      DOUBLE PRECISION V
      DIMENSION V(18, 18)
      common // v

      call schmd(V, 1, 18, 18)
      end

      subroutine DAXPY(N,D,V,M,W,L)
      INTEGER :: N, M, L
      DOUBLE PRECISION D, V(1,1), W(1,1)
      end

      FUNCTION DDOT (N,V,M,W,L)
      INTEGER :: N, M, L
      DOUBLE PRECISION DDOT, V(1,1), W(1,1)
      DDOT = 1
      end

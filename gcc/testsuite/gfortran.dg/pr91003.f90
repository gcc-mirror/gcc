! { dg-do compile }
! { dg-options "-Ofast" }
      SUBROUTINE FOO(N, A, B, C, D, E, F, G)
      COMPLEX A(*)
      LOGICAL H
      INTEGER G
      REAL I, C, J, F, F1, F2, K, E, L, M, B, D
      DO JC = 1, N
        K = F*REAL(A(JC))
        Z = F*AIMAG(A(JC))
        H = .FALSE.
        L = G
        IF(ABS(Z).LT.D .AND. I.GE. MAX(D, B*C, B*J)) THEN
          H = .TRUE.
          L = (D / F1) / MAX(D, F2*I)
        END IF
        IF(ABS(K).LT.D .AND. C.GE. MAX(D, B*I, B*J)) THEN
          L = MAX(L, (D / F1) / MAX(D, F2*C))
        END IF
        IF(ABS(E).LT.D .AND. J.GE. MAX(D, B*C, B*I)) THEN
          H = .TRUE.
          L = MAX(L, (D / BNRM1) / MAX(D, BNRM2*J))
        END IF
        IF(H) THEN
          M = (L*D)*MAX(ABS(K), ABS(Z), ABS(E))
        END IF
        IF(H) THEN
          K = (L*REAL(A(JC)))*F
          Z = (L*AIMAG(A(JC)))*F
        END IF
        A(JC) = CMPLX(K, Z)
      END DO
      END

  SUBROUTINE makeCoulE0(natorb,Coul)
    INTEGER, PARAMETER :: dp=8
    REAL(KIND=dp), PARAMETER :: fourpi=432.42, oorootpi=13413.3142
    INTEGER :: natorb
    REAL(KIND=dp), DIMENSION(45, 45), &
      INTENT(OUT)                            :: Coul
    INTEGER                                  :: gpt, imA, imB, k1, k2, k3, &
                                                k4, lp, mp, np
    REAL(KIND=dp)                            :: alpha, d2f(3,3), &
                                                d4f(3,3,3,3), f, ff, w
    REAL(KIND=dp), DIMENSION(3, 45)          :: M1A
    REAL(KIND=dp), DIMENSION(45)             :: M0A
    DO imA=1, (natorb*(natorb+1))/2
       DO imB=1, (natorb*(natorb+1))/2
          w= M0A(imA)*M0A(imB)
          DO k1=1,3
            w=w+ M1A(k1,imA)*M1A(k1,imB)
          ENDDO
          Coul(imA,imB)=Coul(imA,imB)-4.0_dp*alpha**3*oorootpi*w/3.0_dp
       ENDDO
    ENDDO
  END SUBROUTINE makeCoulE0

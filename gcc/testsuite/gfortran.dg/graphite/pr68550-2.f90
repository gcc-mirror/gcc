! { dg-do compile }
! { dg-options "-floop-nest-optimize -fcheck=bounds -O1" }

SUBROUTINE PD2VAL(RES,NDERIV,TG1,TG2,C0)
    INTEGER, PARAMETER :: dp=8
    REAL(KIND=dp), INTENT(OUT)  :: res(*)
    REAL(KIND=dp), INTENT(IN)   :: TG1, TG2, C0(105,*)
    REAL(KIND=dp)               :: T1(0:13), T2(0:13)
 DO K=1,NDERIV+1
  RES(K)=RES(K)+DOT_PRODUCT(T1(0:7),C0(70:77,K))*T2(6)
  RES(K)=RES(K)+DOT_PRODUCT(T1(0:4),C0(91:95,K))*T2(9)
  RES(K)=RES(K)+DOT_PRODUCT(T1(0:3),C0(96:99,K))*T2(10)
 ENDDO
END SUBROUTINE PD2VAL

! { dg-do compile }
! { dg-options "-floop-nest-optimize -O2 -ffast-math" }
! PR53852 : compile time / memory hog
SUBROUTINE  build_d_tensor_gks(d5f,v,d5)
    INTEGER, PARAMETER :: dp=8
    REAL(KIND=dp),  DIMENSION(3, 3, 3, 3, 3), &
      INTENT(OUT) :: d5f
    REAL(KIND=dp), DIMENSION(3), INTENT(IN)  :: v
    REAL(KIND=dp), INTENT(IN) :: d5
    INTEGER       :: k1, k2, k3, k4, k5
    REAL(KIND=dp) :: w

    d5f = 0.0_dp
    DO k1=1,3
       DO k2=1,3
          DO k3=1,3
             DO k4=1,3
                DO k5=1,3
                   d5f(k5,k4,k3,k2,k1)=d5f(k5,k4,k3,k2,k1)+ &
                          v(k1)*v(k2)*v(k3)*v(k4)*v(k5)*d5
                ENDDO
                w=v(k1)*v(k2)*v(k3)*d4
                d5f(k1,k2,k3,k4,k4)=d5f(k1,k2,k3,k4,k4)+w
                d5f(k1,k2,k4,k3,k4)=d5f(k1,k2,k4,k3,k4)+w
                d5f(k1,k4,k2,k3,k4)=d5f(k1,k4,k2,k3,k4)+w
                d5f(k4,k1,k2,k3,k4)=d5f(k4,k1,k2,k3,k4)+w
                d5f(k1,k2,k4,k4,k3)=d5f(k1,k2,k4,k4,k3)+w
                d5f(k1,k4,k2,k4,k3)=d5f(k1,k4,k2,k4,k3)+w
                d5f(k4,k1,k2,k4,k3)=d5f(k4,k1,k2,k4,k3)+w
                d5f(k1,k4,k4,k2,k3)=d5f(k1,k4,k4,k2,k3)+w
                d5f(k4,k1,k4,k2,k3)=d5f(k4,k1,k4,k2,k3)+w
                d5f(k4,k4,k1,k2,k3)=d5f(k4,k4,k1,k2,k3)+w
             ENDDO
          ENDDO
       ENDDO
    ENDDO
END SUBROUTINE build_d_tensor_gks

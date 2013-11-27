! PR middle-end/57393
! { dg-do compile }
! { dg-options "-g -O2 -ffast-math" }

SUBROUTINE pr57393(nn,e,g,t0,t1,t2,t3,t4,t5,t6,t7,&
                   t8,t9,t10,t11,t12,t13,t14,t15,&
                   t16,t17,t18,t19,t20,t21,t22,t23,&
                   t24,t25,t26,t27,t28,t29,t30,&
                   t31,t32,t33,t34,t35,t36,t37,t38,&
                   t39,t40,t41,t42,t43,t44,t45,t46,t47)
  IMPLICIT REAL*8 (t)
  INTEGER, PARAMETER :: dp=8
  REAL(kind=dp) :: e(nn)
  DO ii=1,nn
     t48 = 0.1955555555e2_dp * t1 * t2 + &
           0.6000000000e1_dp * t3 * t4 * t5
     t49 = 0.1620000000e3_dp * t6 * t7 * t8 + &
           0.1080000000e3_dp * t6 * t9 * t5 - &
           0.6000000000e1_dp * t10 * t20 * t21 * t55 - &
           0.2400000000e2_dp * t10 * t11 * t12 - &
           0.1200000000e2_dp * t13 * t14 * t15
     t50 = t49 + t16
     t51 = (3 * t17 * t18 * t19) + &
           (t22 * t23 * t19) + (t50 * t19) - &
           0.3333333336e0_dp * t24 * t25
     t52 = 0.1555555556e1_dp * t26 * t27 * t12 + &
           (t51 + t28 + t29 + t30) *  &
           0.3125000000e0_dp * t31 * t32 * t33 * t34
     t53 = -0.1000000001e1_dp * t35 * t36 * t5 - &
           (t37 + t38 + t39 + t52) - &
           0.8333333340e-1_dp * t40 * t41 * t42
     t54 = -0.1000000001e1_dp * t43 * t44 * t45 - &
           t47 * (t46 + t53)
     IF (g >= 3 .OR. g == -3) THEN
       e(ii) = e(ii) + t54 * t0
     END IF
  END DO
END SUBROUTINE pr57393

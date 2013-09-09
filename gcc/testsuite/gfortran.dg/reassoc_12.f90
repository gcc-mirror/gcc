! { dg-do compile }
! { dg-options "-O2 -ffast-math" }
! PR middle-end/57370

 SUBROUTINE xb88_lr_adiabatic_lda_calc(e_ndrho_ndrho_ndrho, &
                                       grad_deriv,npoints, sx)
    IMPLICIT REAL*8 (t)
    INTEGER, PARAMETER :: dp=8
    REAL(kind=dp), DIMENSION(1:npoints) :: e_ndrho_ndrho_ndrho, &
                                           e_ndrho_ndrho_rho
      DO ii=1,npoints
          IF( grad_deriv >= 2 .OR. grad_deriv == -2 ) THEN
            t1425 = t233 * t557
            t1429 = beta * t225
            t1622 = t327 * t1621
            t1626 = t327 * t1625
            t1632 = t327 * t1631
            t1685 = t105 * t1684
            t2057 = t1636 + t8 * (t2635 + t3288)
          END IF
          IF( grad_deriv >= 3 .OR. grad_deriv == -3 ) THEN
            t5469 = t5440 - t5443 - t5446 - t5449 - &
                    t5451 - t5454 - t5456 + t5459  - &
                    t5462 + t5466 - t5468
            t5478 = 0.240e2_dp * t1616 * t973 * t645 * t1425
            t5489 = 0.1600000000e2_dp * t1429 * t1658
            t5531 = 0.160e2_dp * t112 * t1626
            t5533 = 0.160e2_dp * t112 * t1632
            t5537 = 0.160e2_dp * t112 * t1622
            t5541 = t5472 - t5478 - t5523 + t5525 + &
                    t5531 + t5533 + t5535 + t5537 + &
                    t5540
            t5565 = t112 * t1685
            t5575 = t5545 - t5548 + t5551 + t5553 - &
                    t5558 + t5560 - t5562 + t5564 - &
                    0.80e1_dp * t5565 + t5568 + t5572 + &
                    t5574
            t5611 = t5579 - t5585 + t5590 - t5595 + &
                    t5597 - t5602 + t5604 + t5607 + &
                    t5610
            t5613 = t5469 + t5541 + t5575 + t5611
            t6223 = t6189 - &
                    0.3333333336e0_dp  * t83 * t84 * t5613 + &
                    t6222
            t6227 = - t8 * (t5305 + t6223)
            e_ndrho_ndrho_rho(ii) = e_ndrho_ndrho_rho(ii) + &
                     t6227 * sx
            t6352 = t5440 - t5443 - t5446 - t5449 - &
                    t5451 - t5454 + &
                    0.40e1_dp * t102  * t327 * t2057 * t557 - &
                    t5456 + t5459 - t5462 + t5466 - &
                    t5468
            t6363 = t5480 - t5489 + &
                    0.9600000000e2_dp  * t1054 * t640 * t3679
            t6367 = t5472 - t5474 - t5478 - t5523 + &
                    t5525 + t5531 + t5533 + t5535 + &
                    t5537 - 0.20e1_dp * t102 * t105 * t6363 + &
                    t5540
            t6370 = t5545 - t5548 + t5551 + t5553 - &
                    t5558 + t5560 - t5562 + t5564  - &
                    0.40e1_dp * t5565 + &
                    t5568 + t5572 + t5574
            t6373 = t5579 - t5585 + t5590 - t5595 + &
                    t5597 - t5602 + t5604 + t5607 + &
                    t5610
            t6375 = t6352 + t6367 + t6370 + t6373
            t6380 = - 0.3333333336e0_dp * t83 * t84 * t6375 + t5701
            t6669 = -t4704 - t8 * (t6344 + t6380 + t6665)
            e_ndrho_ndrho_ndrho(ii) = e_ndrho_ndrho_ndrho(ii) + &
                     t6669 * sx
          END IF
      END DO
  END SUBROUTINE xb88_lr_adiabatic_lda_calc


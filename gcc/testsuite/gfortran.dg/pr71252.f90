
! { dg-do compile }
! { dg-options "-O1 -ffast-math" }

MODULE xc_b97
  INTEGER, PARAMETER :: dp=8
  PRIVATE
  PUBLIC :: b97_lda_info, b97_lsd_info, b97_lda_eval, b97_lsd_eval
CONTAINS
  SUBROUTINE b97_lsd_eval(rho_set,deriv_set,grad_deriv,b97_params)
    INTEGER, INTENT(in)                      :: grad_deriv
    INTEGER                                  :: handle, npoints, param, stat
    LOGICAL                                  :: failure
    REAL(kind=dp)                            :: epsilon_drho, epsilon_rho, &
                                                scale_c, scale_x
    REAL(kind=dp), DIMENSION(:, :, :), POINTER :: dummy, e_0, e_ndra, &
      e_ndra_ndra, e_ndra_ndrb, e_ndra_ra, e_ndra_rb, e_ndrb, e_ndrb_ndrb, &
      e_ndrb_ra, e_ndrb_rb, e_ra, e_ra_ra, e_ra_rb, e_rb, e_rb_rb, &
      norm_drhoa, norm_drhob, rhoa, rhob
    IF (.NOT. failure) THEN
       CALL b97_lsd_calc(&
            rhoa=rhoa, rhob=rhob, norm_drhoa=norm_drhoa,&
            norm_drhob=norm_drhob, e_0=e_0, &
            e_ra=e_ra, e_rb=e_rb, &
            e_ndra=e_ndra, e_ndrb=e_ndrb, &
            e_ra_ra=e_ra_ra, e_ra_rb=e_ra_rb, e_rb_rb=e_rb_rb,&
            e_ra_ndra=e_ndra_ra, e_ra_ndrb=e_ndrb_ra, &
            e_rb_ndrb=e_ndrb_rb, e_rb_ndra=e_ndra_rb,&
            e_ndra_ndra=e_ndra_ndra, e_ndrb_ndrb=e_ndrb_ndrb,&
            e_ndra_ndrb=e_ndra_ndrb,&
            grad_deriv=grad_deriv, npoints=npoints, &
            epsilon_rho=epsilon_rho,epsilon_drho=epsilon_drho,&
            param=param,scale_c_in=scale_c,scale_x_in=scale_x)
    END IF
  END SUBROUTINE b97_lsd_eval
  SUBROUTINE b97_lsd_calc(rhoa, rhob, norm_drhoa, norm_drhob,&
       e_0, e_ra, e_rb, e_ndra, e_ndrb, &
       e_ra_ndra,e_ra_ndrb, e_rb_ndra, e_rb_ndrb,&
       e_ndra_ndra, e_ndrb_ndrb, e_ndra_ndrb, &
       e_ra_ra, e_ra_rb, e_rb_rb,&
       grad_deriv,npoints,epsilon_rho,epsilon_drho, &
       param, scale_c_in, scale_x_in)
    REAL(kind=dp), DIMENSION(*), INTENT(in)  :: rhoa, rhob, norm_drhoa, &
                                                norm_drhob
    REAL(kind=dp), DIMENSION(*), INTENT(inout) :: e_0, e_ra, e_rb, e_ndra, &
      e_ndrb, e_ra_ndra, e_ra_ndrb, e_rb_ndra, e_rb_ndrb, e_ndra_ndra, &
      e_ndrb_ndrb, e_ndra_ndrb, e_ra_ra, e_ra_rb, e_rb_rb
    INTEGER, INTENT(in)                      :: grad_deriv, npoints
    REAL(kind=dp), INTENT(in)                :: epsilon_rho, epsilon_drho
    INTEGER, INTENT(in)                      :: param
    REAL(kind=dp), INTENT(in)                :: scale_c_in, scale_x_in
    REAL(kind=dp) :: A_1, A_2, A_3, alpha_1_1, alpha_1_2, alpha_1_3, alpha_c, &
      t133, t134, t1341, t1348, t1351, t1360, t1368, t138, t1388, t139, &
      u_x_bnorm_drhobnorm_drhob, u_x_brhob, u_x_brhobnorm_drhob, u_x_brhobrhob
    SELECT CASE(grad_deriv)
    CASE default
       DO ii=1,npoints
          IF (rho>epsilon_rho) THEN
             IF (grad_deriv/=0) THEN
                IF (grad_deriv>1 .OR. grad_deriv<-1) THEN
                   alpha_c1rhob = alpha_crhob
                   f1rhob = frhob
                   t1360 = -0.4e1_dp * t105 * t290 * chirhobrhob + (-0.2e1_dp * t239 &
                        * t257 + t709 * t1236 * t711 * t62 / 0.2e1_dp - e_c_u_0rhobrhob) * f&
                        * t108 + t438 * f1rhob * t108 + 0.4e1_dp * t439 * t443 + t1341 * &
                        0.4e1_dp * t1348 * t443 + 0.4e1_dp * t1351 * t443 + 0.12e2_dp * t113&
                        * t107 * t1299 + 0.4e1_dp * t113 * t289 * chirhobrhob
                   IF (grad_deriv>1 .OR. grad_deriv==-2) THEN
                       exc_rhob_rhob = scale_x * (-t4 * t6 / t1152 * gx_b / &
                            0.6e1_dp + e_lsda_x_brhob * (u_x_b1rhob * t31 + u_x_b * u_x_b1rhob *&
                            u_x_brhobrhob * c_x_2)) + scale_c * (((e_c_u_0rhobrhob + (0.2e1_dp *&
                            t726 * t1270 * t278 - t266 * (-t731 * t1205 / 0.4e1_dp + t267 * &
                            t1205 * t647) * t278 - t757 * t1270 * t759 * t80 / 0.2e1_dp) * f * &
                            t110 + alpha_crhob * f1rhob * t110 - 0.4e1_dp * t431 * t435 + &
                            alpha_c1rhob * frhob * t110 + alpha_c * frhobrhob * t110 - 0.4e1_dp &
                            * t433 * t435 - 0.4e1_dp * t1321 * t435 - 0.4e1_dp * t1324 * t435 - &
                            0.12e2_dp * t105 * t796 * t1299 + t1360) * rho + epsilon_c_unifrhob &
                            * c_css_2))
                       e_rb_rb(ii)=e_rb_rb(ii)+exc_rhob_rhob
                   END IF
                END IF ! <1 || >1
             END IF ! /=0
          END IF ! rho>epsilon_rho
       END DO
    END SELECT
  END SUBROUTINE b97_lsd_calc
END MODULE xc_b97


! { dg-do compile }
! { dg-options "-O2 -ffast-math" }

MODULE xc_b97
  INTEGER, PARAMETER :: dp=8
  PRIVATE
  PUBLIC :: b97_lsd_eval
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
      rs_b, rs_brhob, rs_brhobrhob, rsrhoa, rsrhoarhoa, rsrhoarhob, rsrhob, &
      t1014, t102, t1047, t1049, t105, t106, t107
      rsrhoa = -t4 * t212 * t208 / 0.12e2_dp
      t235 = t224 * rsrhoa / 0.2e1_dp + beta_2_1 * rsrhoa + & 
          0.3e1_dp / 0.2e1_dp * t228 * rsrhoa + t50 * t48 * rsrhoa * t232
      t237 = t235 * t236
      e_c_u_0rhoa = -0.2e1_dp * t216 * rsrhoa * t56 + t222 * t237
      epsilon_c_unifrhoa = e_c_u_0rhoa + t285 * t110 + t287 * t110 - &
          t293 + t295 * t108 + t297 * t108 + t301
      e_lsda_c_abrhoa = epsilon_c_unifrhoa * rho + epsilon_c_unif - e_lsda_c_arhoa
      exc_rhoa = scale_x * (e_lsda_x_arhoa * gx_a + e_lsda_x_a * gx_arhoa) + &
          scale_c * (e_lsda_c_abrhoa * gc_ab + e_lsda_c_ab * gc_abrhoa + &
          e_lsda_c_arhoa * gc_a + e_lsda_c_a * gc_arhoa)
      e_ra(ii)=e_ra(ii)+exc_rhoa
  END SUBROUTINE b97_lsd_calc
END MODULE xc_b97

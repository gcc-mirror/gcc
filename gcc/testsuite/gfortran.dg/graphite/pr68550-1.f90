! { dg-do compile }
! { dg-options "-floop-nest-optimize -O2" }

SUBROUTINE integrate_core_1(grid,coef_xyz,pol_x,pol_y,&
             pol_z,map,sphere_bounds,cmax,gridbounds)
    INTEGER, PARAMETER :: dp=8
    INTEGER, INTENT(IN)    :: sphere_bounds(*), cmax, &
                              map(-cmax:cmax,1:3), &
                              gridbounds(2,3)
    REAL(dp), INTENT(IN) :: grid(gridbounds(1,1):gridbounds(2,1), &
             gridbounds(1,2):gridbounds(2,2),&
             gridbounds(1,3):gridbounds(2,3))
    INTEGER, PARAMETER     :: lp = 1
    REAL(dp), INTENT(IN)   :: pol_x(0:lp,-cmax:cmax), &
                              pol_y(1:2,0:lp,-cmax:0), &
                              pol_z(1:2,0:lp,-cmax:0)
    REAL(dp), INTENT(OUT) :: coef_xyz(((lp+1)*(lp+2)*(lp+3))/6)
    INTEGER   :: i, ig, igmax, igmin, j, j2, &
                 jg, jg2, jgmin, k, k2, kg, &
                 kg2, kgmin, lxp, sci
    REAL(dp)  :: coef_x(4,0:lp), &
                 coef_xy(2,((lp+1)*(lp+2))/2), &
                 s(4)
    DO kg=kgmin,0
       DO jg=jgmin,0
          coef_x=0.0_dp
          DO ig=igmin,igmax
             DO lxp=0,lp
                coef_x(:,lxp)=coef_x(:,lxp)+s(:)*pol_x(lxp,ig)
             ENDDO
          END DO
             coef_xy(:,3)=coef_xy(:,3)+coef_x(3:4,0)*pol_y(2,1,jg)
       END DO
                coef_xyz(3)=coef_xyz(3)+coef_xy(1,3)*pol_z(1,0,kg)
    END DO
  END SUBROUTINE integrate_core_1

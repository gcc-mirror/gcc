  IMPLICIT NONE
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
  REAL(KIND=dp)      :: res

  res=exp_radius_very_extended(  0    ,      1   ,       0      ,    1, &
                               (/0.0D0,0.0D0,0.0D0/),&
                               (/1.0D0,0.0D0,0.0D0/),&
                               (/1.0D0,0.0D0,0.0D0/),&
                                 1.0D0,1.0D0,1.0D0,1.0D0)
  if (res.ne.1.0d0) call abort()

CONTAINS

 FUNCTION exp_radius_very_extended(la_min,la_max,lb_min,lb_max,ra,rb,rp,&
                          zetp,eps,prefactor,cutoff) RESULT(radius)

    INTEGER, INTENT(IN)                      :: la_min, la_max, lb_min, lb_max
    REAL(KIND=dp), INTENT(IN)                :: ra(3), rb(3), rp(3), zetp, &
                                                eps, prefactor, cutoff
    REAL(KIND=dp)                            :: radius

    INTEGER                                  :: i, ico, j, jco, la(3), lb(3), &
                                                lxa, lxb, lya, lyb, lza, lzb
    REAL(KIND=dp)                            :: bini, binj, coef(0:20), &
                                                epsin_local, polycoef(0:60), &
                                                prefactor_local, rad_a, &
                                                rad_b, s1, s2

    epsin_local=1.0E-2_dp

    prefactor_local=prefactor*MAX(1.0_dp,cutoff)
    rad_a=SQRT(SUM((ra-rp)**2))
    rad_b=SQRT(SUM((rb-rp)**2))

    polycoef(0:la_max+lb_max)=0.0_dp
    DO lxa=0,la_max
    DO lxb=0,lb_max
       coef(0:la_max+lb_max)=0.0_dp
       bini=1.0_dp
       s1=1.0_dp
       DO i=0,lxa
          binj=1.0_dp
          s2=1.0_dp
          DO j=0,lxb
             coef(lxa+lxb-i-j)=coef(lxa+lxb-i-j) + bini*binj*s1*s2
             binj=(binj*(lxb-j))/(j+1)
             s2=s2*(rad_b)
          ENDDO
          bini=(bini*(lxa-i))/(i+1)
          s1=s1*(rad_a)
       ENDDO
       DO i=0,lxa+lxb
          polycoef(i)=MAX(polycoef(i),coef(i))
       ENDDO
    ENDDO
    ENDDO

    polycoef(0:la_max+lb_max)=polycoef(0:la_max+lb_max)*prefactor_local
    radius=0.0_dp
    DO i=0,la_max+lb_max
          radius=MAX(radius,polycoef(i)**(i+1))
    ENDDO

  END FUNCTION exp_radius_very_extended

END

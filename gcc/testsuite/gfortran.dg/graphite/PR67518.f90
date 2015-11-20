! { dg-do compile }
! { dg-options "-floop-nest-optimize -O2 -ffast-math" }
! PR67518 :  isl: position out of bounds
MODULE ao_util
    INTEGER, PARAMETER :: dp=8
CONTAINS
  FUNCTION exp_radius(l,alpha,threshold,prefactor,epsin) RESULT(radius)
    REAL(KIND=dp), INTENT(IN)                :: alpha, threshold, prefactor
    REAL(KIND=dp), INTENT(IN), OPTIONAL      :: epsin
    DO
       IF (iter.gt.maxiter) THEN
          CALL stop_program(routineN,moduleN,1,"exceeded")
       ENDIF
    ENDDO
    CALL stop_program(routineN,moduleN,1,"exceeded")
  END FUNCTION exp_radius
 FUNCTION exp_radius_very_extended(la_min,la_max,lb_min,lb_max,pab,o1,o2,ra,rb,rp,&
                          zetp,eps,prefactor,cutoff,epsin) RESULT(radius)
    REAL(KIND=dp), DIMENSION(:, :), &
      OPTIONAL, POINTER                      :: pab
    REAL(KIND=dp), INTENT(IN)                :: ra(3), rb(3), rp(3), zetp, &
                                                eps, prefactor, cutoff
    REAL(KIND=dp)                            :: bini, binj, coef(0:20), &
                                                epsin_local, polycoef(0:60), &
                                                rad_b, s1, s2
    IF (PRESENT(pab)) THEN
    ENDIF
    DO lxa=0,la_max
    DO lxb=0,lb_max
       coef(0:la_max+lb_max)=0.0_dp
       DO i=0,lxa
          DO j=0,lxb
             coef(lxa+lxb-i-j)=coef(lxa+lxb-i-j) + bini*binj*s1*s2
          ENDDO
       ENDDO
       DO i=0,lxa+lxb
          polycoef(i)=MAX(polycoef(i),coef(i))
       ENDDO
    ENDDO
    ENDDO
    DO i=0,la_max+lb_max
          radius=MAX(radius,exp_radius(i,zetp,eps,polycoef(i),epsin_local) )
    ENDDO
  END FUNCTION exp_radius_very_extended
END MODULE ao_util

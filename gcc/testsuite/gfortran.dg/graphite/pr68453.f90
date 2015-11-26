! { dg-options "-floop-nest-optimize -O2" }

MODULE dbcsr_geev
  INTEGER, PARAMETER :: real_8=8
CONTAINS
  SUBROUTINE dbcsr_dgeev(jobvl,jobvr,matrix,ndim,evals,revec,levec)
    CHARACTER(1)                             :: jobvl, jobvr
    REAL(real_8), DIMENSION(:, :)            :: matrix
    INTEGER                                  :: ndim
    COMPLEX(real_8), DIMENSION(:)            :: evals
    COMPLEX(real_8), DIMENSION(:, :)         :: revec, levec
    INTEGER                                  :: i, info, lwork
    REAL(real_8)                             :: norm, tmp_array(ndim,ndim), &
                                                work(20*ndim)
    REAL(real_8), DIMENSION(ndim)            :: eval1, eval2
    REAL(real_8), DIMENSION(ndim, ndim)      :: evec_l, evec_r
    DO WHILE (i.le.ndim)
      IF(ABS(eval2(i)).LT.EPSILON(REAL(0.0,real_8)))THEN
        norm=SQRT(SUM(evec_r(:,i)**2.0_real_8)+SUM(evec_r(:,i+1)**2.0_real_8))
        revec(:,i)=CMPLX(evec_r(:,i),evec_r(:,i+1),real_8)/norm
      END IF
    END DO
  END SUBROUTINE  dbcsr_dgeev
END MODULE dbcsr_geev

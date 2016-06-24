! PR debug/71642
! { dg-do compile }
! { dg-options "-g" }

MODULE gauss_colloc
  INTEGER, PARAMETER :: dp=8
CONTAINS
SUBROUTINE collocGauss(h,h_inv,grid,poly,alphai,posi,max_r2,&
        periodic,gdim,local_bounds,local_shift,poly_shift,scale,lgrid,error)
    REAL(dp), DIMENSION(0:, 0:, 0:), &
      INTENT(inout)                          :: grid
    INTEGER, INTENT(inout), OPTIONAL :: lgrid
    CONTAINS
    SUBROUTINE kloop6
    IF (kJump/=1 .AND. (ikstart+kmax-kstart>=ndim(2)+l_shift(2) .OR.&
        ikstart2+kmin-kstart2<=l_ub(2)-ndim(2))) THEN
        DO
            DO k=kstart2,kend2,-1
                IF ( PRESENT ( lgrid ) ) THEN
                  grid(ik,ij,ii) = grid(ik,ij,ii) + p_v*res_k
                END IF
            END DO
        END DO
    END IF
    END SUBROUTINE
END SUBROUTINE
END MODULE gauss_colloc

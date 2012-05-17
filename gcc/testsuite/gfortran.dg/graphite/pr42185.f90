! { dg-do compile }
! { dg-options "-fgraphite -O -ffast-math" }

MODULE powell
  INTEGER, PARAMETER :: dp=8
CONTAINS
  SUBROUTINE trsapp (n,npt,xopt,xpt,gq,hq,pq,delta,step,d,g,hd,hs,crvmin)
    REAL(dp), DIMENSION(*), INTENT(INOUT)    :: step, d, g, hd, hs
    LOGICAL                                  :: jump1, jump2
    REAL(dp) :: alpha, angle, angtest, bstep, cf, cth, dd, delsq, dg, dhd, &
      reduc, sg, sgk, shs, ss, sth, temp, tempa, tempb
    DO i=1,n
       dd=dd+d(i)**2
    END DO
    mainloop : DO
       IF ( .NOT. jump2 ) THEN
          IF ( .NOT. jump1 ) THEN
             bstep=temp/(ds+SQRT(ds*ds+dd*temp))
             IF (alpha < bstep) THEN
                IF (ss < delsq) CYCLE mainloop
             END IF
             IF (gg <= 1.0e-4_dp*ggbeg) EXIT mainloop
          END IF
       END IF
    END DO mainloop
  END SUBROUTINE trsapp
END MODULE powell

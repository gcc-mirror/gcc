! { dg-options "-O1 -fgraphite" }

MODULE powell
  INTEGER, PARAMETER :: dp=8
CONTAINS
  SUBROUTINE newuob (n,npt,x,rhobeg,rhoend,maxfun,xbase,&
       xopt,xnew,xpt,fval,gq,hq,pq,bmat,zmat,ndim,d,vlag,w,opt)
    REAL(dp), DIMENSION(npt, *), &
      INTENT(inout)                          :: xpt
    REAL(dp), DIMENSION(*), INTENT(inout)    :: fval, gq, hq, pq
120 IF (dsq <= 1.0e-3_dp*xoptsq) THEN
       DO k=1,npt
          DO i=1,n
             gq(i)=gq(i)+temp*xpt(k,i)
          END DO
       END DO
    END IF
  END SUBROUTINE newuob
END MODULE powell
! { dg-final { cleanup-modules "powell" } }

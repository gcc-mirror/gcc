! { dg-options "-O2 -g" }

MODULE powell
  INTEGER, PARAMETER :: dp=8
CONTAINS
  SUBROUTINE newuob (n,  bmat,  ndim,  d,  vlag,  w, npt)
    REAL(dp), DIMENSION(ndim, *), INTENT(inout) :: bmat
    REAL(dp), DIMENSION(*), INTENT(inout)    :: d, vlag, w
    REAL(dp) :: sum
    INTEGER, INTENT(in) :: npt
    DO j=1,n
       jp=npt+j
       DO k=1,n
          sum=sum+bmat(jp,k)*d(k)
       END DO
       vlag(jp)=sum
    END DO
  END SUBROUTINE newuob
END MODULE powell

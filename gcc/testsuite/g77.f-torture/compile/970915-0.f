* fixed by patch to safe_from_p to avoid visiting any SAVE_EXPR
* node twice in a given top-level call to it.
* (JCB com.c patch of 1998-06-04.)

      SUBROUTINE TSTSIG11
      IMPLICIT COMPLEX (A-Z)
      EXTERNAL gzi1,gzi2
      branch3 =  sw2 / cw
     .     * (  rdw * (epsh*gzi1(A,B)-gzi2(A,B))
     .     + rdw * (epsh*gzi1(A,B)-gzi2(A,B)) )
     .     + (-1./2. + 2.*sw2/3.) / (sw*cw)
     .     * rdw * (epsh*gzi1(A,B)-gzi2(A,B)
     .     + rdw * (epsh*gzi1(A,B)-gzi2(A,B))
     .     + rdw * (epsh*gzi1(A,B)-gzi2(A,B)) )
     .     * rup * (epsh*gzi1(A,B)-gzi2(A,B)
     .     + rup * (epsh*gzi1(A,B)-gzi2(A,B)) )
     .     * 4.*(3.-tw**2) * gzi2(A,B)
     .     + ((1.+2./tauw)*tw**2-(5.+2./tauw))* gzi1(A,B)
      RETURN
      END

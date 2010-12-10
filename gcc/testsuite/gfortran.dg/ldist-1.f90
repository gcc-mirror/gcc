! { dg-do compile }     
! { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" }

Subroutine PADEC(DKS,DKDS,HVAR,WM,WG,FN,NS,AN,BN,CN,IT)
  IMPLICIT REAL*8 (A-H, O-Z)
  DIMENSION DKS(*),DKDS(*),HVAR(*)
  COMPLEX*16 WM(*),WG(*),FN(*),AN(*),BN(*),CN(*)
  COMPLEX*16 H2,CONST
  COMMON/STRCH/ALP,BET,DH,ZH,UG,VG,T1,T2,DT,TOL,ALPHA ,HAMP,BUMP
  Parameter (F1 = .8333333333333333D0, F2 = .0833333333333333D0)

  SS=DT/(2.0D0)

  do J=2,NS
     BS=SS*DKS(J)*HVAR(J)*HVAR(J)
     AN(J)=F1+2.*BS
     BN(J)=F2-BS
     CN(J)=F2-BS
     H2=WM(J+1)

     if(J.EQ.NS) then
        CONST=CN(J)*H2
     else
        CONST=(0.D0,0.D0)
     endif
     FN(J)=(BS+F2)*(H2)+(F1-2.D0*BS)-CONST
  end do

  return
end Subroutine PADEC

! There are 5 legal partitions in this code.  Based on the data
! locality heuristic, this loop should not be split.

! { dg-final { scan-tree-dump-not "distributed: split to" "ldist" } }
! { dg-final { cleanup-tree-dump "ldist" } }

! { dg-do compile }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 69154 - inlined matmul used to cause an ICE inside a WHERE.
MODULE m_numeric_tools
 INTEGER, PARAMETER :: dp=8
CONTAINS
subroutine llsfit_svd(xx,yy,sigma,nfuncs,funcs,chisq,par,var,cov,info)
 real(dp),intent(in) :: xx(:),yy(:),sigma(:)
 real(dp),dimension(SIZE(xx)) :: bb,sigm1
 real(dp) :: tmp(nfuncs)
 real(dp),allocatable :: work(:),Vt(:,:),U(:,:),S(:)
 real(dp),dimension(3,3) :: a, b, c
 WHERE (S>TOL_*MAXVAL(S))
  tmp=MATMUL(bb,U)/S
 END WHERE
 call random_number(a)
 call random_number(b)
 c = matmul(a,b)
end subroutine llsfit_svd

END MODULE m_numeric_tools
! { dg-final { scan-tree-dump-times "matmul_r8" 1 "original" } }

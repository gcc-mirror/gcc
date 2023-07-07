! { dg-do compile }
! { dg-options "-fdump-tree-optimized-blocks-details -O3" }

SUBROUTINE S42(a,b,c,N)
 IMPLICIT NONE
 integer :: N
 real*8  :: a(N),b(N),c(N),tmp,tmp2,tmp4
 real*8, parameter :: p=1.0D0/3.0D0
 integer :: i
 c=0.0D0
 DO i=1,N
   tmp=a(i)**p ! could even be done with a cube root
   tmp2=tmp*tmp
   tmp4=tmp2*tmp2
   b(i)=b(i)+tmp4
   c(i)=c(i)+tmp2
 ENDDO
END SUBROUTINE
! { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } }

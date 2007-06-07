! PR 32220, ICE when the loop is not unrolled enough to eliminate all 
!   register copies
! { dg-do compile }
! { dg-options "-O3" }

      subroutine derv (b,cosxy,thick)
c
      common /shell4/xji(3,3)
c
      dimension cosxy(6,*),
     1          thick(*),b(*)
c

      do 125 i=1,3
      b(k2+i)=xji(i,1) + xji(i,2) + xji(i,3)
  125 b(k3+i)=cosxy(i+3,kk) + cosxy(i,kk)
c
c
      return
      end

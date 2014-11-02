C PR rtl-optimization/42246
C { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } }
C { dg-options "-O2 -fselective-scheduling -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops" }

      subroutine distance(x,clo)
      implicit real*8 (a-h,o-z)
      dimension x(2,6),x1(2,6),clo(6)
      do 60 i=1,2
        do 20 j=1,6
          x(i,j)=clo(j)
   20   continue
        do 40 iq=1,6
          x1(i,iq)=0.0d0
   40   continue
        do 50 j=1,6
          x(i,j)=x1(i,j)
   50   continue
   60 continue
      return
      end


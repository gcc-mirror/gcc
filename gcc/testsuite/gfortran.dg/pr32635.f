C { dg-do run }
C PR 32635 - this used to call an ICE in verify_ssa at -O2.
C An empty main program ensures that we cycle through all
C the options.

      program main
      end

      subroutine aled7(ix,ib,itable,ip,ip2,imat,nummat,
     1                 mx0,k,numnp,numel,iadj)

      implicit double precision (a-h,o-z)                                    dp

      common/cale6/fst(16,4),ist(256,14)
c
      dimension ib(*),itable(*),ip(3,*),ip2(*),ix(6,*),imat(nummat+1,*)
c
c
      ipnt=1
      do 20 i=1,numel
      if (imat(ix(5,i),mx0).ne.1) go to 20
   20 continue
c
      k=0
      kflg=0
   25 do 30 i=1,ipnt
      if (ip(1,i).eq.0) go to 30
      ii=i
      go to 40
   30 continue
c
   40 k=k+1
      iel=ip(3,ii)
      ib(k+iadj)=i1
      if (kflg.eq.1) ip(1,ii)=0
      kflg=1
c
      isum=0
      do 50 i=1,ipnt
      if (ip(1,i).eq.0) isum=isum+1
      if (ip(1,i).eq.0.or.ip(1,i).ne.i2) go to 50
      ii=i
      if (ip(3,i).eq.iel) go to 40
   50 continue
c
      if (ip(1,ii).eq.i2) go to 40
      kflg=0
      if (isum.ne.ipnt) go to 25
c
      return
      end

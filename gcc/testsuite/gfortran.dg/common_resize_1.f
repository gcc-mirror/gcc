c { dg-do run }
c { dg-options "-std=legacy" }
c
c Tests the fix for PR32302, in which the resizing of 'aux32' would cause
c misalignment for double precision types and a wrong result would be obtained
c at any level of optimization except none.
c
c Contributed by Dale Ranta <dir@lanl.gov> 
c
      subroutine unpki(ixp,nwcon,nmel)
      parameter(lnv=32)
      implicit double precision (a-h,o-z)                                    dp
c
c     unpack connection data
c
      common/aux32/kka(lnv),kkb(lnv),kkc(lnv),
     1 kk1(lnv),kk2(lnv),kk3(lnv),dxy(lnv),
     2 dyx(lnv),dyz(lnv),dzy(lnv),dzx(lnv),
     3 dxz(lnv),vx17(lnv),vx28(lnv),vx35(lnv),
     4 vx46(lnv),vy17(lnv),vy28(lnv),
     5 vy35(lnv),vy46(lnv),vz17(lnv),vz28(lnv),vz35(lnv),vz46(lnv)
      common/aux33/ix1(lnv),ix2(lnv),ix3(lnv),ix4(lnv),ix5(lnv),
     1             ix6(lnv),ix7(lnv),ix8(lnv),mxt(lnv)
      dimension ixp(nwcon,*)
c
      return
      end
      subroutine prtal
      parameter(lnv=32)
      implicit double precision (a-h,o-z)                                    dp
      common/aux8/
     & x1(lnv),x2(lnv),x3(lnv),x4(lnv),
     & x5(lnv),x6(lnv),x7(lnv),x8(lnv),
     & y1(lnv),y2(lnv),y3(lnv),y4(lnv),
     & y5(lnv),y6(lnv),y7(lnv),y8(lnv),
     & z1(lnv),z2(lnv),z3(lnv),z4(lnv),
     & z5(lnv),z6(lnv),z7(lnv),z8(lnv)
      common/aux9/vlrho(lnv),det(lnv)
      common/aux10/
     1 px1(lnv),px2(lnv),px3(lnv),px4(lnv),
     & px5(lnv),px6(lnv),px7(lnv),px8(lnv),
     2 py1(lnv),py2(lnv),py3(lnv),py4(lnv),
     & py5(lnv),py6(lnv),py7(lnv),py8(lnv),
     3 pz1(lnv),pz2(lnv),pz3(lnv),pz4(lnv),
     & pz5(lnv),pz6(lnv),pz7(lnv),pz8(lnv),
     4 vx1(lnv),vx2(lnv),vx3(lnv),vx4(lnv),
     5 vx5(lnv),vx6(lnv),vx7(lnv),vx8(lnv),
     6 vy1(lnv),vy2(lnv),vy3(lnv),vy4(lnv),
     7 vy5(lnv),vy6(lnv),vy7(lnv),vy8(lnv),
     8 vz1(lnv),vz2(lnv),vz3(lnv),vz4(lnv),
     9 vz5(lnv),vz6(lnv),vz7(lnv),vz8(lnv)
      common/aux32/    ! { dg-warning "shall be of the same size" }
     a a17(lnv),a28(lnv),dett(lnv),
     1 aj1(lnv),aj2(lnv),aj3(lnv),aj4(lnv),
     2 aj5(lnv),aj6(lnv),aj7(lnv),aj8(lnv),
     3 aj9(lnv),x17(lnv),x28(lnv),x35(lnv),
     4 x46(lnv),y17(lnv),y28(lnv),y35(lnv),
     5 y46(lnv),z17(lnv),z28(lnv),z35(lnv),z46(lnv)
      common/aux33/    ! { dg-warning "shall be of the same size" }
     a ix1(lnv),ix2(lnv),ix3(lnv),ix4(lnv),ix5(lnv),
     1             ix6(lnv),ix7(lnv),ix8(lnv),mxt(lnv),nmel
      common/aux36/lft,llt
      common/failu/sieu(lnv),failu(lnv)
      common/sand1/ihf,ibemf,ishlf,itshf
      dimension aj5968(lnv),aj6749(lnv),aj4857(lnv),aji1(lnv),aji2(lnv),
     1          aji3(lnv),aji4(lnv),aji5(lnv),
     1          aji6(lnv),aji7(lnv),aji8(lnv),aji9(lnv),aj12(lnv),
     2          aj45(lnv),aj78(lnv),b17(lnv),b28(lnv),c17(lnv),c28(lnv)
c
      equivalence (x17,aj5968),(x28,aj6749),(x35,aj4857),(x46,aji1),
     1 (y17,aji2),(y28,aji3),(y35,aji4),(y46,aji5),(z17,aji6),
     2 (z28,aji7),(z35,aji8),(z46,aji9),(aj1,aj12),(aj2,aj45),
     3 (aj3,aj78),(px1,b17),(px2,b28),(px3,c17),(px4,c28)
      data o64th/0.0156250/
c
c     jacobian matrix
c
      do 10 i=lft,llt
      x17(i)=x7(i)-x1(i)
      x28(i)=x8(i)-x2(i)
      x35(i)=x5(i)-x3(i)
      x46(i)=x6(i)-x4(i)
      y17(i)=y7(i)-y1(i)
      y28(i)=y8(i)-y2(i)
      y35(i)=y5(i)-y3(i)
      y46(i)=y6(i)-y4(i)
      z17(i)=z7(i)-z1(i)
      z28(i)=z8(i)-z2(i)
      z35(i)=z5(i)-z3(i)
   10 z46(i)=z6(i)-z4(i)
      do 20 i=lft,llt
      aj1(i)=x17(i)+x28(i)-x35(i)-x46(i)
      aj2(i)=y17(i)+y28(i)-y35(i)-y46(i)
      aj3(i)=z17(i)+z28(i)-z35(i)-z46(i)
      a17(i)=x17(i)+x46(i)
      a28(i)=x28(i)+x35(i)
      b17(i)=y17(i)+y46(i)
      b28(i)=y28(i)+y35(i)
      c17(i)=z17(i)+z46(i)
   20 c28(i)=z28(i)+z35(i)
      do 30 i=lft,llt
      aj4(i)=a17(i)+a28(i)
      aj5(i)=b17(i)+b28(i)
      aj6(i)=c17(i)+c28(i)
      aj7(i)=a17(i)-a28(i)
      aj8(i)=b17(i)-b28(i)
   30 aj9(i)=c17(i)-c28(i)
c
c     jacobian
c
      do 40 i=lft,llt
      aj5968(i)=aj5(i)*aj9(i)-aj6(i)*aj8(i)
      aj6749(i)=aj6(i)*aj7(i)-aj4(i)*aj9(i)
   40 aj4857(i)=aj4(i)*aj8(i)-aj5(i)*aj7(i)
      if (ihf.ne.1) then
      do 50 i=lft,llt
   50 det(i)=o64th*(aj1(i)*aj5968(i)+aj2(i)*aj6749(i)+aj3(i)*aj4857(i))
      else
      do 55 i=lft,llt
      det(i)=o64th*(aj1(i)*aj5968(i)+aj2(i)*aj6749(i)+aj3(i)*aj4857(i))
     1       *failu(i) + (1. - failu(i))
   55 continue
      endif
      do 60 i=lft,llt
   60 dett(i)=o64th/det(i)

      if (det(lft) .ne. 1d0) call abort ()
      if (det(llt) .ne. 1d0) call abort ()

      return
c
      end
      program main
      parameter(lnv=32)
      implicit double precision (a-h,o-z)                                    dp
      common/aux8/
     & x1(lnv),x2(lnv),x3(lnv),x4(lnv),
     & x5(lnv),x6(lnv),x7(lnv),x8(lnv),
     & y1(lnv),y2(lnv),y3(lnv),y4(lnv),
     & y5(lnv),y6(lnv),y7(lnv),y8(lnv),
     & z1(lnv),z2(lnv),z3(lnv),z4(lnv),
     & z5(lnv),z6(lnv),z7(lnv),z8(lnv)
      common/aux36/lft,llt
      common/sand1/ihf,ibemf,ishlf,itshf
      lft=1
      llt=1
      x1(1)=0
      x2(1)=1
      x3(1)=1
      x4(1)=0
      x5(1)=0
      x6(1)=1
      x7(1)=1
      x8(1)=0

      y1(1)=0
      y2(1)=0
      y3(1)=1
      y4(1)=1
      y5(1)=0
      y6(1)=0
      y7(1)=1
      y8(1)=1

      z1(1)=0
      z2(1)=0
      z3(1)=0
      z4(1)=0
      z5(1)=1
      z6(1)=1
      z7(1)=1
      z8(1)=1
      call prtal
      stop
      end


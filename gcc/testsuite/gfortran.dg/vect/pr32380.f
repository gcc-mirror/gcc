! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-additional-options "-O3 -fcray-pointer" }
! PR 32380 - loops were not vectorized due to unaligned store.
      subroutine trnfbt(e,f,qs,mte,gm,ihgenf,hgener,lft,llt,sthick,
     .                  fibl,istupd,ies,hoff)
      parameter (nlq=96)
      integer nnlq
      common/newnlq/nnlq
c   ... implicit common ...
      integer imauto,iteopt,lauto,mthsol,ilimit,maxref,icnvrg,
     & igdiv,nwebuf,neql,neqt,imterm,imphas,nbfgs,
     & numupd,istif,itrlas,imerr,imdof,neqtgl,lsmtd,lsdir
      common/bki01i/imauto,iteopt,lauto,mthsol,ilimit,maxref,icnvrg,
     & igdiv,nwebuf,neql,neqt,imterm,imphas,nbfgs,
     & numupd,istif,itrlas,imerr,imdof,neqtgl,lsmtd,lsdir
      REAL dtimp,dtimp0,timeim,dtmnim,dtmxim,cvtl,ectl,rctl,
     & tolls,dnorm2,dtprnt,dtplot,dtiter,dtrefm
      common/bki01r/dtimp,dtimp0,timeim,dtmnim,dtmxim,cvtl,ectl,rctl,
     & tolls,dnorm2,dtprnt(2),dtplot(2),dtiter(2),dtrefm(2)
      REAL ascntl
      common/bki02r/ascntl(150)
      logical lsensw
      common/bki01l/lsensw(20)
      integer imip,isolvr,icwrb
      common/bki02i/imip(100),isolvr(200),icwrb(50)
c   ... implicit common ...
c
c
c
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc
      common/bki03iloc/lnodim(nlq,16),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc
      real*4 ske
      common/bki03rloc/ske(nlq,1176)
      integer lmke
      common/bki04iloc/lmke(nlq,48)
c******************************************************************
c|  livermore software technology corporation  (lstc)             |
c|  ------------------------------------------------------------  |
c|  copyright 1987,1988,1989 john o. hallquist, lstc              |
c|  all rights reserved                                           |
c******************************************************************
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
c
      common/bk12loc/b12,b2,qhg,qhgm,qhgb,qhgw
      common/aux00loc/
     & sig1m(nlq),sig2m(nlq),sig4m(nlq),sig1n(nlq),sig2n(nlq),
     & sig4n(nlq),sig5n(nlq),sig6n(nlq),sig5l(nlq),sig6l(nlq),
     & str33(nlq),enginc(nlq)
      common/aux01loc/
     &ft11(nlq),ft12(nlq),ft13(nlq),ft21(nlq),ft22(nlq),ft23(nlq),
     &fm11(nlq),fm12(nlq),fm21(nlq),fm22(nlq),
     &fm31(nlq),fm32(nlq),fm41(nlq),fm42(nlq),
     &fmr11(nlq),fmr12(nlq),fmr21(nlq),fmr22(nlq),fmr31(nlq),
     &fmr32(nlq),fmr41(nlq),fmr42(nlq),sg5(nlq),sg6(nlq)
      common/aux7loc/
     1 vx1(nlq),vx2(nlq),vx3(nlq),vx4(nlq),
     2 vx5(nlq),vx6(nlq),vx7(nlq),vx8(nlq),
     3 vy1(nlq),vy2(nlq),vy3(nlq),vy4(nlq),
     4 vy5(nlq),vy6(nlq),vy7(nlq),vy8(nlq),
     5 vz1(nlq),vz2(nlq),vz3(nlq),vz4(nlq),
     6 vz5(nlq),vz6(nlq),vz7(nlq),vz8(nlq)
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux11loc/
     &ft31(nlq),ft32(nlq),ft33(nlq),ft41(nlq),ft42(nlq),ft43(nlq),
     &htx(nlq),hty(nlq),gm1(nlq),gm2(nlq),gm3(nlq),gm4(nlq),
     &bsum(nlq),qhx(nlq),qhy(nlq),qwz(nlq),qtx(nlq),qty(nlq)
      real*4 mx1,my1,mz1,mx2,my2,mz2,mx3,my3,mz3,mx4,my4,mz4
      common/aux13loc/
     &zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),gl23(nlq),
     &gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq),
     &fx1(nlq),fy1(nlq),fz1(nlq),fx2(nlq),fy2(nlq),fz2(nlq),
     &fx3(nlq),fy3(nlq),fz3(nlq),fx4(nlq),fy4(nlq),fz4(nlq),
     &mx1(nlq),my1(nlq),mz1(nlq),mx2(nlq),my2(nlq),mz2(nlq),
     &mx3(nlq),my3(nlq),mz3(nlq),mx4(nlq),my4(nlq),mz4(nlq)
      common/aux33loc/
     1 ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ixs(nlq,4),mxt(nlq)
      common/aux35loc/rhoa(nlq),cxx(nlq),fcl(nlq),fcq(nlq)
      common/hourgloc/ymod(nlq),gmod(nlq),ifsv(nlq)
      common/soundloc/sndspd(nlq),sndsp(nlq),diagm(nlq),sarea(nlq),
     . dxl(nlq)
      common/bel6loc/bm(nlq,3,8),bb(nlq,3,8),bs(nlq,2,12),bhg(nlq,4),
     1 ex(nlq,3,8),dp0(nlq,3,3),dp1(nlq,3,3),dp2(nlq,3,3),
     2 ds(nlq),dhg(nlq,5)
c
      common/shlioc/ioshl(60)
      common/failuloc/sieu(nlq),fail(nlq),ifaili(nlq)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 ncs10,ncs11,ncs12,ncs13,ncs14,ncs15,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsavloc/savfrc(nlq,24),svfail(nlq),ndof,ifail
      common/sorterloc/nnc,lczc
      common/sorter/znnc,zlczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,
     & nh11,nh12,nh13,nh14,nh15,nh16,
     & nt11,nt12,nt13,nt14,nt15,nt16,
     & nb11,nb12,nb13,nb14,nb15,nb16,
     & nu11,nu12,nu13,nu14,nu15,nu16,
     & nd11,nd12,nd13,nd14,nd15,nd16 
      common/subtssloc/dt1siz(nlq)
      common/matflr/mtfail(200)
      common/berwcmloc/xll(nlq),rigx(nlq),rigy(nlq)
      common /mem/ mp
      integer ia(1)
      pointer(mp,ia)
      real*4 mmode,ies
      dimension e(3,1),f(3,1),qs(9,1),gm(4,*),hgener(*)
      dimension qs1(nlq),qs2(nlq),qs3(nlq),qs4(nlq),qs5(nlq)
      dimension fibl(9,1),sthick(*),ies(*),hoff(*)
c
      ifail=0
      if (qhgb+qhgw+qhgm.gt.1.e-04) then
      tmode=qhgb*ymod(lft)/1920.0
      wmode=qhgw*gmod(lft)/120.00
      mmode=qhgm*ymod(lft)/80.000
c
      hgfac=rhoa(lft)*sndspd(lft)
c
      do i=lft,llt
      htxi  =area(i)*(x3(i)-x2(i)-x4(i))
      htyi  =area(i)*(y3(i)-y2(i)-y4(i))
      gm1(i)= 1.-px1(i)*htxi-py1(i)*htyi  
      gm2(i)=-1.-px2(i)*htxi-py2(i)*htyi  
      gm3(i)= 2.-gm1(i)
      gm4(i)=-2.-gm2(i)
      qhx(i)=gm2(i)*vx2(i)+gm3(i)*vx3(i)+gm4(i)*vx4(i)
      qhy(i)=gm2(i)*vy2(i)+gm3(i)*vy3(i)+gm4(i)*vy4(i)
      qwz(i)=gm2(i)*vz2(i)+gm3(i)*vz3(i)+gm4(i)*vz4(i)
      enddo
      do i=lft,llt
      c3=      sqrt(abs(sarea(i)))*thick(i)/(dt1siz(i)+1.e-16)
      c2=(hgfac*qhgw)*c3
      c1=(hgfac*qhgb*.01)*c3*thick(i)*thick(i)
      c3=(hgfac*qhgm)*c3
      qtx(i)=gm2(i)*vx6(i)+gm3(i)*vx7(i)+gm4(i)*vx8(i)
      qty(i)=gm2(i)*vy6(i)+gm3(i)*vy7(i)+gm4(i)*vy8(i)
      xll2  =2.*xll(i)
      qhxi  =qhx(i)+xll2*rigy(i)
      qhyi  =qhy(i)-xll2*rigx(i)
      qs1(i)=c3*qhxi
      qs2(i)=c3*qhyi
      qs3(i)=c2*qwz(i)
      qs4(i)=c1*qtx(i)
      qs5(i)=c1*qty(i)
      enddo
c
c
c
      if (isolvr(18).eq.0) then
c
      do i=lft,llt
      fm11(i)= fm11(i)+gm1(i)*qs4(i)
      fm12(i)= fm12(i)+gm1(i)*qs5(i)
      fm21(i)= fm21(i)+gm2(i)*qs4(i)
      fm22(i)= fm22(i)+gm2(i)*qs5(i)
      fm31(i)= fm31(i)+gm3(i)*qs4(i)
      fm32(i)= fm32(i)+gm3(i)*qs5(i)
      fm41(i)= fm41(i)+gm4(i)*qs4(i)
      fm42(i)= fm42(i)+gm4(i)*qs5(i)
      enddo
C
      else
c
      do 45 i=lft,llt
      ft31(i)=-ft11(i)+gm3(i)*qs1(i)
      ft32(i)=-ft12(i)+gm3(i)*qs2(i)
      ft33(i)=-ft13(i)+gm3(i)*qs3(i)
      ft41(i)=-ft21(i)+gm4(i)*qs1(i)
      ft42(i)=-ft22(i)+gm4(i)*qs2(i)
      ft43(i)=-ft23(i)+gm4(i)*qs3(i)
      ft11(i)= ft11(i)+gm1(i)*qs1(i)
      ft12(i)= ft12(i)+gm1(i)*qs2(i)
      ft13(i)= ft13(i)+gm1(i)*qs3(i)
      ft21(i)= ft21(i)+gm2(i)*qs1(i)
      ft22(i)= ft22(i)+gm2(i)*qs2(i)
      ft23(i)= ft23(i)+gm2(i)*qs3(i)
      fm11(i)= fm11(i)+gm1(i)*qs4(i)
      fm12(i)= fm12(i)+gm1(i)*qs5(i)
      fm21(i)= fm21(i)+gm2(i)*qs4(i)
      fm22(i)= fm22(i)+gm2(i)*qs5(i)
      fm31(i)= fm31(i)+gm3(i)*qs4(i)
      fm32(i)= fm32(i)+gm3(i)*qs5(i)
      fm41(i)= fm41(i)+gm4(i)*qs4(i)
      fm42(i)= fm42(i)+gm4(i)*qs5(i)
   45 continue
      endif
c
      else
c
      do 40 i=lft,llt
      ft31(i)=-ft11(i)
      ft32(i)=-ft12(i)
      ft33(i)=-ft13(i)
      ft41(i)=-ft21(i)
      ft42(i)=-ft22(i)
      ft43(i)=-ft23(i)
   40 continue
      endif
c
c
      do i=lft,llt
      mz1(i)=gl31(i)*fm11(i)+gl32(i)*fm12(i)
      mz2(i)=gl31(i)*fm21(i)+gl32(i)*fm22(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)+gl33(i)*ft13(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)+gl33(i)*ft23(i)
      mz3(i)=gl31(i)*fm31(i)+gl32(i)*fm32(i)
      mz4(i)=gl31(i)*fm41(i)+gl32(i)*fm42(i)
      fz3(i)=gl31(i)*ft31(i)+gl32(i)*ft32(i)+gl33(i)*ft33(i)
      fz4(i)=gl31(i)*ft41(i)+gl32(i)*ft42(i)+gl33(i)*ft43(i)
      enddo
   90 continue
c
      if (output) then
      do i=lft,llt
      savfrc(i, 1)= fx1(i)
      savfrc(i, 2)= fy1(i)
      enddo
c
      ndof=4
      if (ifail.eq.1) then
        do i=lft,llt
        svfail(i)=fail(i)
        enddo
      endif
      endif
c
      return
      end

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { ! vect_element_align } } } }
! { dg-final { scan-tree-dump-times "vectorized 5 loops" 1 "vect" { target { vect_element_align && { ! vect_call_sqrtf } } } } }
! { dg-final { scan-tree-dump-times "vectorized 6 loops" 1 "vect" { target { vect_element_align && vect_call_sqrtf } } } }

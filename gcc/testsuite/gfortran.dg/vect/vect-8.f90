! { dg-do compile }
! { dg-require-effective-target vect_double }
! { dg-additional-options "-fno-tree-loop-distribute-patterns -finline-matmul-limit=0" }

module lfk_prec
 integer, parameter :: dp=kind(1.d0)
end module lfk_prec

!***********************************************

SUBROUTINE kernel(tk)
!***********************************************************************
!                                                                      *
!            KERNEL     executes 24 samples of Fortran computation     *
!               TK(1) - total cpu time to execute only the 24 kernels. *
!               TK(2) - total Flops executed by the 24 Kernels         *
!***********************************************************************
!                                                                      *
!     L. L. N. L.   F O R T R A N   K E R N E L S:   M F L O P S       *
!                                                                      *
!   These kernels measure  Fortran  numerical  computation rates for a *
!   spectrum of  CPU-limited  computational  structures.  Mathematical *
!   through-put is measured  in  units  of  millions of floating-point *
!   operations executed per Second, called Mega-Flops/Sec.             *
!                                                                      *
!   This program  measures  a realistic  CPU performance range for the *
!   Fortran programming system  on  a  given day.  The CPU performance *
!   rates depend  strongly  on  the maturity of the Fortran compiler's *
!   ability to translate Fortran code into efficient machine code.     *
!   [ The CPU hardware  capability  apart  from  compiler maturity (or *
!   availability), could be measured (or simulated) by programming the *
!   kernels in assembly  or machine code directly.  These measurements *
!   can also  serve  as a framework for tracking the maturation of the *
!   Fortran compiler during system development.]                       *
!                                                                      *
!     Fonzi's Law: There is not now and there never will be a language *
!                  in which it is the least bit difficult to write     *
!                  bad programs.                                       *
!                                                    F.H.MCMAHON  1972 *
!***********************************************************************

!     l1 :=  param-dimension governs the size of most 1-d arrays
!     l2 :=  param-dimension governs the size of most 2-d arrays

!     Loop :=  multiple pass control to execute kernel long enough to ti
!    me.
!     n  :=  DO loop control for each kernel.  Controls are set in subr.
!     SIZES

!     ******************************************************************
use lfk_prec
implicit double precision  (a-h,o-z)
!IBM  IMPLICIT  REAL*8           (A-H,O-Z)

REAL(kind=dp), INTENT(inout)                        :: tk
INTEGER :: test !!,AND

COMMON/alpha/mk,ik,im,ml,il,mruns,nruns,jr,iovec,npfs(8,3,47)
COMMON/beta/tic,times(8,3,47),see(5,3,8,3),terrs(8,3,47),csums(8,3  &
    ,47),fopn(8,3,47),dos(8,3,47)

COMMON/spaces/ion,j5,k2,k3,loop1,laps,loop,m,kr,lp,n13h,ibuf,nx,l,  &
    npass,nfail,n,n1,n2,n13,n213,n813,n14,n16,n416,n21,nt1,nt2,last,idebug  &
    ,mpy,loop2,mucho,mpylim,intbuf(16)

COMMON/spacer/a11,a12,a13,a21,a22,a23,a31,a32,a33,ar,br,c0,cr,di,dk  &
    ,dm22,dm23,dm24,dm25,dm26,dm27,dm28,dn,e3,e6,expmax,flx,q,qa,r,ri  &
    ,s,scale,sig,stb5,t,xnc,xnei,xnm

COMMON/space0/time(47),csum(47),ww(47),wt(47),ticks,fr(9),terr1(47  &
    ),sumw(7),start,skale(47),bias(47),ws(95),total(47),flopn(47),iq(7  &
    ),npf,npfs1(47)

COMMON/spacei/wtp(3),mul(3),ispan(47,3),ipass(47,3)

!     ******************************************************************


INTEGER :: e,f,zone
COMMON/ispace/e(96),f(96),ix(1001),ir(1001),zone(300)

COMMON/space1/u(1001),v(1001),w(1001),x(1001),y(1001),z(1001),g(1001)  &
    ,du1(101),du2(101),du3(101),grd(1001),dex(1001),xi(1001),ex(1001)  &
    ,ex1(1001),dex1(1001),vx(1001),xx(1001),rx(1001),rh(2048),vsp(101)  &
    ,vstp(101),vxne(101),vxnd(101),ve3(101),vlr(101),vlin(101),b5(101)  &
    ,plan(300),d(300),sa(101),sb(101)

COMMON/space2/p(4,512),px(25,101),cx(25,101),vy(101,25),vh(101,7),  &
    vf(101,7),vg(101,7),vs(101,7),za(101,7),zp(101,7),zq(101,7),zr(101  &
    ,7),zm(101,7),zb(101,7),zu(101,7),zv(101,7),zz(101,7),b(64,64),c(64,64)  &
    ,h(64,64),u1(5,101,2),u2(5,101,2),u3(5,101,2)

!     ******************************************************************

dimension zx(1023),xz(447,3),tk(6),mtmp(1)
EQUIVALENCE(zx(1),z(1)),(xz(1,1),x(1))
double precision temp
logical ltmp


!     ******************************************************************

!     STANDARD PRODUCT COMPILER DIRECTIVES MAY BE USED FOR OPTIMIZATION





CALL trace('KERNEL  ')

CALL SPACE

mpy= 1
mpysav= mpylim
loop2= 1
mpylim= loop2
l= 1
loop= 1
lp= loop
it0= test(0)
loop2= mpysav
mpylim= loop2
do

!***********************************************************************
!***  KERNEL 1      HYDRO FRAGMENT
!***********************************************************************

  x(:n)= q+y(:n)*(r*zx(11:n+10)+t*zx(12:n+11))
IF(test(1) <= 0)THEN
  EXIT
END IF
END DO

do
!                   we must execute    DO k= 1,n  repeatedly for accurat
!    e timing

!***********************************************************************
!***  KERNEL 2      ICCG EXCERPT (INCOMPLETE CHOLESKY - CONJUGATE GRADIE
!    NT)
!***********************************************************************


ii= n
ipntp= 0

do while(ii >  1)
ipnt= ipntp
ipntp= ipntp+ii
ii= ishft(ii,-1)
i= ipntp+1
!dir$ vector always
       x(ipntp+2:ipntp+ii+1)=x(ipnt+2:ipntp:2)-v(ipnt+2:ipntp:2) &
     &*x(ipnt+1:ipntp-1:2)-v(ipnt+3:ipntp+1:2)*x(ipnt+3:ipntp+1:2)
END DO
IF(test(2) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 3      INNER PRODUCT
!***********************************************************************


q= dot_product(z(:n),x(:n))
IF(test(3) <= 0)THEN
  EXIT
END IF
END DO
m= (1001-7)/2

!***********************************************************************
!***  KERNEL 4      BANDED LINEAR EQUATIONS
!***********************************************************************

fw= 1.000D-25

do
!dir$ vector always
 xz(6,:3)= y(5)*(xz(6,:3)+matmul(y(5:n:5), xz(:n/5,:3)))

IF(test(4) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 5      TRI-DIAGONAL ELIMINATION, BELOW DIAGONAL (NO VECTORS
!    )
!***********************************************************************


tmp= x(1)
DO i= 2,n
  tmp= z(i)*(y(i)-tmp)
  x(i)= tmp
END DO
IF(test(5) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 6      GENERAL LINEAR RECURRENCE EQUATIONS
!***********************************************************************


DO i= 2,n
  w(i)= 0.0100D0+dot_product(b(i,:i-1),w(i-1:1:-1))
END DO
IF(test(6) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 7      EQUATION OF STATE FRAGMENT
!***********************************************************************


  x(:n)= u(:n)+r*(z(:n)+r*y(:n))+t*(u(4:n+3)+r*(u(3:n+2)+r*u(2:n+1))+t*(  &
      u(7:n+6)+q*(u(6:n+5)+q*u(5:n+4))))
IF(test(7) <= 0)THEN
  EXIT
END IF
END DO

do


!***********************************************************************
!***  KERNEL 8      A.D.I. INTEGRATION
!***********************************************************************


nl1= 1
nl2= 2
fw= 2.000D0
  DO ky= 2,n
DO kx= 2,4
    du1ky= u1(kx,ky+1,nl1)-u1(kx,ky-1,nl1)
    du2ky= u2(kx,ky+1,nl1)-u2(kx,ky-1,nl1)
    du3ky= u3(kx,ky+1,nl1)-u3(kx,ky-1,nl1)
    u1(kx,ky,nl2)= u1(kx,ky,nl1)+a11*du1ky+a12*du2ky+a13  &
        *du3ky+sig*(u1(kx+1,ky,nl1)-fw*u1(kx,ky,nl1)+u1(kx-1,ky,nl1))
    u2(kx,ky,nl2)= u2(kx,ky,nl1)+a21*du1ky+a22*du2ky+a23  &
        *du3ky+sig*(u2(kx+1,ky,nl1)-fw*u2(kx,ky,nl1)+u2(kx-1,ky,nl1))
    u3(kx,ky,nl2)= u3(kx,ky,nl1)+a31*du1ky+a32*du2ky+a33  &
        *du3ky+sig*(u3(kx+1,ky,nl1)-fw*u3(kx,ky,nl1)+u3(kx-1,ky,nl1))
  END DO
END DO
IF(test(8) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 9      INTEGRATE PREDICTORS
!***********************************************************************


  px(1,:n)= dm28*px(13,:n)+px(3,:n)+dm27*px(12,:n)+dm26*px(11,:n)+dm25*px(10  &
      ,:n)+dm24*px(9,:n)+dm23*px(8,:n)+dm22*px(7,:n)+c0*(px(5,:n)+px(6,:n))
IF(test(9) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 10     DIFFERENCE PREDICTORS
!***********************************************************************

!dir$ unroll(2)
	  do k= 1,n
	      br= cx(5,k)-px(5,k)
	      px(5,k)= cx(5,k)
	      cr= br-px(6,k)
	      px(6,k)= br
	      ar= cr-px(7,k)
	      px(7,k)= cr
	      br= ar-px(8,k)
	      px(8,k)= ar
	      cr= br-px(9,k)
	      px(9,k)= br
	      ar= cr-px(10,k)
	      px(10,k)= cr
	      br= ar-px(11,k)
	      px(11,k)= ar
	      cr= br-px(12,k)
	      px(12,k)= br
	      px(14,k)= cr-px(13,k)
	      px(13,k)= cr
	    enddo
IF(test(10) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 11     FIRST SUM.   PARTIAL SUMS.              (NO VECTORS)
!***********************************************************************


temp= 0
DO k= 1,n
  temp= temp+y(k)
  x(k)= temp
END DO
IF(test(11) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 12     FIRST DIFF.
!***********************************************************************

  x(:n)= y(2:n+1)-y(:n)
IF(test(12) <= 0)THEN
  EXIT
END IF
END DO
fw= 1.000D0

!***********************************************************************
!***  KERNEL 13      2-D PIC   Particle In Cell
!***********************************************************************


do

! rounding modes for integerizing make no difference here
	  do k= 1,n
	      i1= 1+iand(int(p(1,k)),63)
	      j1= 1+iand(int(p(2,k)),63)
	      p(3,k)= p(3,k)+b(i1,j1)
	      p(1,k)= p(1,k)+p(3,k)
	      i2= iand(int(p(1,k)),63)
	      p(1,k)= p(1,k)+y(i2+32)
	      p(4,k)= p(4,k)+c(i1,j1)
	      p(2,k)= p(2,k)+p(4,k)
	      j2= iand(int(p(2,k)),63)
	      p(2,k)= p(2,k)+z(j2+32)
	      i2= i2+e(i2+32)
	      j2= j2+f(j2+32)
	      h(i2,j2)= h(i2,j2)+fw
	    enddo
IF(test(13) <= 0)THEN
  EXIT
END IF
END DO
fw= 1.000D0

!***********************************************************************
!***  KERNEL 14      1-D PIC   Particle In Cell
!***********************************************************************



do

  ix(:n)= grd(:n)
!dir$ ivdep
  vx(:n)= ex(ix(:n))-ix(:n)*dex(ix(:n))
  ir(:n)= vx(:n)+flx
  rx(:n)= vx(:n)+flx-ir(:n)
  ir(:n)= iand(ir(:n),2047)+1
  xx(:n)= rx(:n)+ir(:n)
DO k= 1,n
      rh(ir(k))= rh(ir(k))+fw-rx(k)
      rh(ir(k)+1)= rh(ir(k)+1)+rx(k)
END DO
IF(test(14) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 15     CASUAL FORTRAN.  DEVELOPMENT VERSION.
!***********************************************************************


!       CASUAL ORDERING OF SCALAR OPERATIONS IS TYPICAL PRACTICE.
!       THIS EXAMPLE DEMONSTRATES THE NON-TRIVIAL TRANSFORMATION
!       REQUIRED TO MAP INTO AN EFFICIENT MACHINE IMPLEMENTATION.


ng= 7
nz= n
ar= 0.05300D0
br= 0.07300D0
!$omp parallel do private(t,j,k,r,s,i,ltmp) if(nz>98)
do j= 2,ng-1
  do k= 2,nz
    i= merge(k-1,k,vf(k,j) <  vf((k-1),j))
    t= merge(br,ar,vh(k,(j+1)) <= vh(k,j))
    r= MAX(vh(i,j),vh(i,j+1))
    s= vf(i,j)
    vy(k,j)= t/s*SQRT(vg(k,j)**2+r*r)
    if(k < nz)then
	ltmp=vf(k,j) >= vf(k,(j-1))
	i= merge(j,j-1,ltmp)
	t= merge(ar,br,ltmp)
	r= MAX(vg(k,i),vg(k+1,i))
	s= vf(k,i)
	vs(k,j)= t/s*SQRT(vh(k,j)**2+r*r)
    endif
  END do
  vs(nz,j)= 0.0D0
END do
  vy(2:nz,ng)= 0.0D0
IF(test(15) <= 0)THEN
  EXIT
END IF
END DO
ii= n/3

!***********************************************************************
!***  KERNEL 16     MONTE CARLO SEARCH LOOP
!***********************************************************************

lb= ii+ii
k2= 0
k3= 0

do
DO m= 1,zone(1)
  j2= (n+n)*(m-1)+1
  DO k= 1,n
    k2= k2+1
    j4= j2+k+k
    j5= zone(j4)
    IF(j5 >= n)THEN
      IF(j5 == n)THEN
        EXIT
      END IF
      k3= k3+1
      IF(d(j5) <  d(j5-1)*(t-d(j5-2))**2+(s-d(j5-3))**2+ (r-d(j5-4))**2)THEN
        go to 200
      END IF
      IF(d(j5) == d(j5-1)*(t-d(j5-2))**2+(s-d(j5-3))**2+ (r-d(j5-4))**2)THEN
        EXIT
      END IF
    ELSE
      IF(j5-n+lb <  0)THEN
        IF(plan(j5) <  t)THEN
          go to 200
        END IF
        IF(plan(j5) == t)THEN
          EXIT
        END IF
      ELSE
        IF(j5-n+ii <  0)THEN
          IF(plan(j5) <  s)THEN
            go to 200
          END IF
          IF(plan(j5) == s)THEN
            EXIT
          END IF
        ELSE
          IF(plan(j5) <  r)THEN
            go to 200
          END IF
          IF(plan(j5) == r)THEN
            EXIT
          END IF
        END IF
      END IF
    END IF
    IF(zone(j4-1) <= 0)THEN
      go to 200
    END IF
  END DO
  EXIT
  200             IF(zone(j4-1) == 0)THEN
    EXIT
  END IF
END DO
IF(test(16) <= 0)THEN
  EXIT
END IF
END DO
dw= 5.0000D0/3.0000D0

!***********************************************************************
!***  KERNEL 17     IMPLICIT, CONDITIONAL COMPUTATION       (NO VECTORS)
!***********************************************************************

!         RECURSIVE-DOUBLING VECTOR TECHNIQUES CAN NOT BE USED
!         BECAUSE CONDITIONAL OPERATIONS APPLY TO EACH ELEMENT.

fw= 1.0000D0/3.0000D0
tw= 1.0300D0/3.0700D0

do
scale= dw
rtmp= fw
e6= tw
DO k= n,2,-1
  e3= rtmp*vlr(k)+vlin(k)
  xnei= vxne(k)
  vxnd(k)= e6
  xnc= scale*e3
!                                      SELECT MODEL
  IF(max(rtmp,xnei) <= xnc)THEN
!                                      LINEAR MODEL
    ve3(k)= e3
    rtmp= e3+e3-rtmp
    vxne(k)= e3+e3-xnei
  ELSE
    rtmp= rtmp*vsp(k)+vstp(k)
!                                      STEP MODEL
    vxne(k)= rtmp
    ve3(k)= rtmp
  END IF
    e6= rtmp
END DO
xnm= rtmp
IF(test(17) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 18     2-D EXPLICIT HYDRODYNAMICS FRAGMENT
!***********************************************************************


t= 0.003700D0
s= 0.004100D0
kn= 6
jn= n
  zb(2:jn,2:kn)=(zr(2:jn,2:kn)+zr(2:jn,:kn-1))/(zm(2:jn,2:kn)+zm(:jn-1,2:kn)) &
	*(zp(:jn-1,2:kn)-zp(2:jn,2:kn)+(zq(:jn-1,2:kn)-zq(2:jn,2:kn)))
  za(2:jn,2:kn)=(zr(2:jn,2:kn)+zr(:jn-1,2:kn))/(zm(:jn-1,2:kn)+zm(:jn-1,3:kn+1))  &
	*(zp(:jn-1,3:kn+1)-zp(:jn-1,2:kn)+(zq(:jn-1,3:kn+1)-zq(:jn-1,2:kn)))
  zu(2:jn,2:kn)= zu(2:jn,2:kn)+ &
	s*(za(2:jn,2:kn)*(zz(2:jn,2:kn)-zz(3:jn+1,2:kn)) &
	-za(:jn-1,2:kn)*(zz(2:jn,2:kn)-zz(:jn-1,2:kn)) &
	-zb(2:jn,2:kn)*(zz(2:jn,2:kn)-zz(2:jn,:kn-1))+ &
	zb(2:jn,3:kn+1)*(zz(2:jn, 2:kn)-zz(2:jn,3:kn+1)))
  zv(2:jn,2:kn)= zv(2:jn,2:kn)+ &
	s*(za(2:jn,2:kn)*(zr(2:jn,2:kn)-zr(3:jn+1,2:kn)) &
	-za(:jn-1,2:kn)*(zr(2:jn,2:kn)-zr(:jn-1,2:kn)) &
	-zb(2:jn,2:kn)*(zr(2:jn,2:kn)-zr(2:jn,:kn-1))+ &
	zb(2:jn,3:kn+1)*(zr(2:jn, 2:kn)-zr(2:jn,3:kn+1)))
  zr(2:jn,2:kn)= zr(2:jn,2:kn)+t*zu(2:jn,2:kn)
  zz(2:jn,2:kn)= zz(2:jn,2:kn)+t*zv(2:jn,2:kn)
IF(test(18) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 19      GENERAL LINEAR RECURRENCE EQUATIONS    (NO VECTORS)
!***********************************************************************

kb5i= 0

DO k= 1,n
  b5(k+kb5i)= sa(k)+stb5*sb(k)
  stb5= b5(k+kb5i)-stb5
END DO
DO k= n,1,-1
  b5(k+kb5i)= sa(k)+stb5*sb(k)
  stb5= b5(k+kb5i)-stb5
END DO
IF(test(19) <= 0)THEN
  EXIT
END IF
END DO
dw= 0.200D0

!***********************************************************************
!***  KERNEL 20     DISCRETE ORDINATES TRANSPORT: RECURRENCE (NO VECTORS
!***********************************************************************


do

rtmp= xx(1)
DO k= 1,n
  di= y(k)*(rtmp+dk)-g(k)
  dn=merge( max(s,min(z(k)*(rtmp+dk)/di,t)),dw,di /= 0.0)
  x(k)= ((w(k)+v(k)*dn)*rtmp+u(k))/(vx(k)+v(k)*dn)
  rtmp= ((w(k)-vx(k))*rtmp+u(k))*DN/(vx(k)+v(k)*dn)+ rtmp
 xx(k+1)= rtmp
END DO
IF(test(20) <= 0)THEN
  EXIT
END IF
END DO

do

!***********************************************************************
!***  KERNEL 21     MATRIX*MATRIX PRODUCT
!***********************************************************************

    px(:25,:n)= px(:25,:n)+matmul(vy(:25,:25),cx(:25,:n))
IF(test(21) <= 0)THEN
  EXIT
END IF
END DO
expmax= 20.0000D0


!***********************************************************************
!***  KERNEL 22     PLANCKIAN DISTRIBUTION
!***********************************************************************

!      EXPMAX= 234.500d0
fw= 1.00000D0
u(n)= 0.99000D0*expmax*v(n)

do

  y(:n)= u(:n)/v(:n)
  w(:n)= x(:n)/(EXP(y(:n))-fw)
IF(test(22) <= 0)THEN
  EXIT
END IF
END DO
fw= 0.17500D0

!***********************************************************************
!***  KERNEL 23     2-D IMPLICIT HYDRODYNAMICS FRAGMENT
!***********************************************************************


do

      DO k= 2,n
	 do j=2,6
	     za(k,j)= za(k,j)+fw*(za(k,j+1)*zr(k,j)-za(k,j)+		&
     &		zv(k,j)*za(k-1,j)+(zz(k,j)+za(k+1,j)*			&
     &		zu(k,j)+za(k,j-1)*zb(k,j)))
      END DO
    END DO
IF(test(23) <= 0)THEN
  EXIT
END IF
END DO
x(n/2)= -1.000D+10

!***********************************************************************
!***  KERNEL 24     FIND LOCATION OF FIRST MINIMUM IN ARRAY
!***********************************************************************

!      X( n/2)= -1.000d+50

do
 m= minloc(x(:n),DIM=1)

IF(test(24) == 0)THEN
  EXIT
END IF
END DO
sum= 0.00D0
som= 0.00D0
DO k= 1,mk
  sum= sum+time(k)
  times(jr,il,k)= time(k)
  terrs(jr,il,k)= terr1(k)
  npfs(jr,il,k)= npfs1(k)
  csums(jr,il,k)= csum(k)
  dos(jr,il,k)= total(k)
  fopn(jr,il,k)= flopn(k)
  som= som+flopn(k)*total(k)
END DO
tk(1)= tk(1)+sum
tk(2)= tk(2)+som
!                        Dumpout Checksums:  file "chksum"
!     WRITE ( 7,706) jr, il
! 706 FORMAT(1X,2I3)
!     WRITE ( 7,707) ( CSUM(k), k= 1,mk)
! 707 FORMAT(5X,'&',1PE23.16,',',1PE23.16,',',1PE23.16,',')

CALL track('KERNEL  ')
RETURN
END SUBROUTINE kernel

! { dg-final { scan-tree-dump-times "vectorized 23 loops" 1 "vect" { target aarch64*-*-* } } }
! { dg-final { scan-tree-dump-times "vectorized 2\[23\] loops" 1 "vect" { target { vect_intdouble_cvt && { ! aarch64*-*-* } } } } }
! { dg-final { scan-tree-dump-times "vectorized 17 loops" 1 "vect" { target { { ! vect_intdouble_cvt } && { ! aarch64*-*-* } } } } }

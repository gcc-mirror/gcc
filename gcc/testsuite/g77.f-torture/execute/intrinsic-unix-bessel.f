c  intrinsic-unix-bessel.f
c
c Test Bessel function intrinsics.  
c These functions are only available if provided by system
c
c     David Billinghurst <David.Billinghurst@riotinto.com>
c
      real x, a
      double precision dx, da
      integer i
      integer*2 j
      integer*1 k
      integer*8 m
      logical fail
      common /flags/ fail
      fail = .false.

      x = 2.0
      dx = x 
      i = 2
      j = i
      k = i
      m = i
c     BESJ0  - Bessel function of first kind of order zero
      a = 0.22389077
      da = a
      call c_r(BESJ0(x),a,'BESJ0(real)')
      call c_d(BESJ0(dx),da,'BESJ0(double)')
      call c_d(DBESJ0(dx),da,'DBESJ0(double)')

c     BESJ1  - Bessel function of first kind of order one
      a = 0.57672480
      da = a
      call c_r(BESJ1(x),a,'BESJ1(real)')
      call c_d(BESJ1(dx),da,'BESJ1(double)')
      call c_d(DBESJ1(dx),da,'DBESJ1(double)')

c     BESJN  - Bessel function of first kind of order N
      a = 0.3528340
      da = a
      call c_r(BESJN(i,x),a,'BESJN(integer,real)')
      call c_r(BESJN(j,x),a,'BESJN(integer*2,real)')
      call c_r(BESJN(k,x),a,'BESJN(integer*1,real)')
      call c_d(BESJN(i,dx),da,'BESJN(integer,double)')
      call c_d(BESJN(j,dx),da,'BESJN(integer*2,double)')
      call c_d(BESJN(k,dx),da,'BESJN(integer*1,double)')
      call c_d(DBESJN(i,dx),da,'DBESJN(integer,double)')
      call c_d(DBESJN(j,dx),da,'DBESJN(integer*2,double)')
      call c_d(DBESJN(k,dx),da,'DBESJN(integer*1,double)')

c     BESY0  - Bessel function of second kind of order zero
      a = 0.51037567
      da = a
      call c_r(BESY0(x),a,'BESY0(real)')
      call c_d(BESY0(dx),da,'BESY0(double)')
      call c_d(DBESY0(dx),da,'DBESY0(double)')

c     BESY1  - Bessel function of second kind of order one
      a = 0.-0.1070324
      da = a
      call c_r(BESY1(x),a,'BESY1(real)')
      call c_d(BESY1(dx),da,'BESY1(double)')
      call c_d(DBESY1(dx),da,'DBESY1(double)')

c     BESYN  - Bessel function of second kind of order N
      a = -0.6174081
      da = a
      call c_r(BESYN(i,x),a,'BESYN(integer,real)')
      call c_r(BESYN(j,x),a,'BESYN(integer*2,real)')
      call c_r(BESYN(k,x),a,'BESYN(integer*1,real)')
      call c_d(BESYN(i,dx),da,'BESYN(integer,double)')
      call c_d(BESYN(j,dx),da,'BESYN(integer*2,double)')
      call c_d(BESYN(k,dx),da,'BESYN(integer*1,double)')
      call c_d(DBESYN(i,dx),da,'DBESYN(integer,double)')
      call c_d(DBESYN(j,dx),da,'DBESYN(integer*2,double)')
      call c_d(DBESYN(k,dx),da,'DBESYN(integer*1,double)')

      if ( fail ) call abort()
      end

      subroutine failure(label)
c     Report failure and set flag
      character*(*) label
      logical fail
      common /flags/ fail
      write(6,'(a,a,a)') 'Test ',label,' FAILED'
      fail = .true.
      end

      subroutine c_r(a,b,label)
c     Check if REAL a equals b, and fail otherwise
      real a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0e-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end

      subroutine c_d(a,b,label)
c     Check if DOUBLE PRECISION a equals b, and fail otherwise
      double precision a, b
      character*(*) label
      if ( abs(a-b) .gt. 1.0d-5 ) then
         call failure(label)
         write(6,*) 'Got ',a,' expected ', b
      end if
      end

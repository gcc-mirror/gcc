c { dg-do run }
c  f90-intrinsic-bit.f
c
c Test Fortran 90 
c  * intrinsic bit manipulation functions - Section 13.10.10
c  * bitcopy subroutine - Section 13.9.3 
c David Billinghurst <David.Billinghurst@riotinto.com>
c
c Notes: 
c  * g77 only supports scalar arguments
c  * third argument of ISHFTC is not optional in g77

      logical fail
      integer   i, i2, ia, i3
      integer(kind=2) j, j2, j3, ja
      integer(kind=1) k, k2, k3, ka
      integer(kind=8) m, m2, m3, ma

      common /flags/ fail
      fail = .false.

c     BIT_SIZE - Section 13.13.16
c     Determine BIT_SIZE by counting the bits 
      ia = 0
      i = 0
      i = not(i)
      do while ( (i.ne.0) .and. (ia.lt.127) ) 
         ia = ia + 1
         i = ishft(i,-1)
      end do
      call c_i(BIT_SIZE(i),ia,'BIT_SIZE(integer)')
      ja = 0
      j = 0
      j = not(j)
      do while  ( (j.ne.0) .and. (ja.lt.127) ) 
         ja = ja + 1
         j = ishft(j,-1)
      end do
      call c_i2(BIT_SIZE(j),ja,'BIT_SIZE(integer(2))')
      ka = 0
      k = 0
      k = not(k)
      do while ( (k.ne.0) .and. (ka.lt.127) )
         ka = ka + 1
         k = ishft(k,-1)
      end do
      call c_i1(BIT_SIZE(k),ka,'BIT_SIZE(integer(1))')
      ma = 0
      m = 0
      m = not(m)
      do while ( (m.ne.0) .and. (ma.lt.127) )
         ma = ma + 1
         m = ishft(m,-1)
      end do
      call c_i8(BIT_SIZE(m),ma,'BIT_SIZE(integer(8))')

c     BTEST  - Section 13.13.17
      j  = 7
      j2 = 3
      k  = 7
      k2 = 3
      m  = 7
      m2 = 3
      call c_l(BTEST(7,3),.true.,'BTEST(integer,integer)')
      call c_l(BTEST(7,j2),.true.,'BTEST(integer,integer(2))')
      call c_l(BTEST(7,k2),.true.,'BTEST(integer,integer(1))')
      call c_l(BTEST(7,m2),.true.,'BTEST(integer,integer(8))')
      call c_l(BTEST(j,3),.true.,'BTEST(integer(2),integer)')
      call c_l(BTEST(j,j2),.true.,'BTEST(integer(2),integer(2))')
      call c_l(BTEST(j,k2),.true.,'BTEST(integer(2),integer(1))')
      call c_l(BTEST(j,m2),.true.,'BTEST(integer(2),integer(8))')
      call c_l(BTEST(k,3),.true.,'BTEST(integer(1),integer)')
      call c_l(BTEST(k,j2),.true.,'BTEST(integer(1),integer(2))')
      call c_l(BTEST(k,k2),.true.,'BTEST(integer(1),integer(1))')
      call c_l(BTEST(k,m2),.true.,'BTEST(integer(1),integer(8))')
      call c_l(BTEST(m,3),.true.,'BTEST(integer(8),integer)')
      call c_l(BTEST(m,j2),.true.,'BTEST(integer(8),integer(2))')
      call c_l(BTEST(m,k2),.true.,'BTEST(integer(8),integer(1))')
      call c_l(BTEST(m,m2),.true.,'BTEST(integer(8),integer(8))')
 
c     IAND   - Section 13.13.40
      j  = 3
      j2 = 1
      ja = 1
      k  = 3
      k2 = 1
      ka = 1
      m  = 3
      m2 = 1
      ma = 1
      call c_i(IAND(3,1),1,'IAND(integer,integer)')
      call c_i2(IAND(j,j2),ja,'IAND(integer(2),integer(2)')
      call c_i1(IAND(k,k2),ka,'IAND(integer(1),integer(1))')
      call c_i8(IAND(m,m2),ma,'IAND(integer(8),integer(8))')


c     IBCLR  - Section 13.13.41
      j  = 14
      j2 = 1
      ja = 12
      k  = 14
      k2 = 1
      ka = 12
      m  = 14
      m2 = 1
      ma = 12
      call c_i(IBCLR(14,1),12,'IBCLR(integer,integer)')
      call c_i(IBCLR(14,j2),12,'IBCLR(integer,integer(2))')
      call c_i(IBCLR(14,k2),12,'IBCLR(integer,integer(1))')
      call c_i(IBCLR(14,m2),12,'IBCLR(integer,integer(8))')
      call c_i2(IBCLR(j,1),ja,'IBCLR(integer(2),integer)')
      call c_i2(IBCLR(j,j2),ja,'IBCLR(integer(2),integer(2))')
      call c_i2(IBCLR(j,k2),ja,'IBCLR(integer(2),integer(1))')
      call c_i2(IBCLR(j,m2),ja,'IBCLR(integer(2),integer(8))')
      call c_i1(IBCLR(k,1),ka,'IBCLR(integer(1),integer)')
      call c_i1(IBCLR(k,j2),ka,'IBCLR(integer(1),integer(2))')
      call c_i1(IBCLR(k,k2),ka,'IBCLR(integer(1),integer(1))')
      call c_i1(IBCLR(k,m2),ka,'IBCLR(integer(1),integer(8))')
      call c_i8(IBCLR(m,1),ma,'IBCLR(integer(8),integer)')
      call c_i8(IBCLR(m,j2),ma,'IBCLR(integer(8),integer(2))')
      call c_i8(IBCLR(m,k2),ma,'IBCLR(integer(8),integer(1))')
      call c_i8(IBCLR(m,m2),ma,'IBCLR(integer(8),integer(8))')

c     IBSET  - Section 13.13.43
      j  = 12
      j2 = 1
      ja = 14
      k  = 12
      k2 = 1
      ka = 14
      m  = 12
      m2 = 1
      ma = 14
      call c_i(IBSET(12,1),14,'IBSET(integer,integer)')
      call c_i(IBSET(12,j2),14,'IBSET(integer,integer(2))')
      call c_i(IBSET(12,k2),14,'IBSET(integer,integer(1))')
      call c_i(IBSET(12,m2),14,'IBSET(integer,integer(8))')
      call c_i2(IBSET(j,1),ja,'IBSET(integer(2),integer)')
      call c_i2(IBSET(j,j2),ja,'IBSET(integer(2),integer(2))')
      call c_i2(IBSET(j,k2),ja,'IBSET(integer(2),integer(1))')
      call c_i2(IBSET(j,m2),ja,'IBSET(integer(2),integer(8))')
      call c_i1(IBSET(k,1),ka,'IBSET(integer(1),integer)')
      call c_i1(IBSET(k,j2),ka,'IBSET(integer(1),integer(2))')
      call c_i1(IBSET(k,k2),ka,'IBSET(integer(1),integer(1))')
      call c_i1(IBSET(k,m2),ka,'IBSET(integer(1),integer(8))')
      call c_i8(IBSET(m,1),ma,'IBSET(integer(8),integer)')
      call c_i8(IBSET(m,j2),ma,'IBSET(integer(8),integer(2))')
      call c_i8(IBSET(m,k2),ma,'IBSET(integer(8),integer(1))')
      call c_i8(IBSET(m,m2),ma,'IBSET(integer(8),integer(8))')

c     IEOR   - Section 13.13.45
      j  = 3
      j2 = 1
      ja = 2
      k  = 3
      k2 = 1
      ka = 2
      m  = 3
      m2 = 1
      ma = 2
      call c_i(IEOR(3,1),2,'IEOR(integer,integer)')
      call c_i2(IEOR(j,j2),ja,'IEOR(integer(2),integer(2))')
      call c_i1(IEOR(k,k2),ka,'IEOR(integer(1),integer(1))')
      call c_i8(IEOR(m,m2),ma,'IEOR(integer(8),integer(8))')

c     ISHFT  - Section 13.13.49
      i  = 3
      i2 = 1
      i3 = 0
      ia = 6
      j  = 3
      j2 = 1
      j3 = 0
      ja = 6
      k  = 3
      k2 = 1
      k3 = 0
      ka = 6
      m  = 3
      m2 = 1
      m3 = 0
      ma = 6
      call c_i(ISHFT(i,i2),ia,'ISHFT(integer,integer)')
      call c_i(ISHFT(i,BIT_SIZE(i)),i3,'ISHFT(integer,integer) 2')
      call c_i(ISHFT(i,-BIT_SIZE(i)),i3,'ISHFT(integer,integer) 3')
      call c_i(ISHFT(i,0),i,'ISHFT(integer,integer) 4')
      call c_i2(ISHFT(j,j2),ja,'ISHFT(integer(2),integer(2))')
      call c_i2(ISHFT(j,BIT_SIZE(j)),j3,
     $     'ISHFT(integer(2),integer(2)) 2')
      call c_i2(ISHFT(j,-BIT_SIZE(j)),j3,
     $     'ISHFT(integer(2),integer(2)) 3')
      call c_i2(ISHFT(j,0),j,'ISHFT(integer(2),integer(2)) 4')
      call c_i1(ISHFT(k,k2),ka,'ISHFT(integer(1),integer(1))')
      call c_i1(ISHFT(k,BIT_SIZE(k)),k3,
     $     'ISHFT(integer(1),integer(1)) 2')
      call c_i1(ISHFT(k,-BIT_SIZE(k)),k3,
     $     'ISHFT(integer(1),integer(1)) 3')
      call c_i1(ISHFT(k,0),k,'ISHFT(integer(1),integer(1)) 4')
      call c_i8(ISHFT(m,m2),ma,'ISHFT(integer(8),integer(8))')
      call c_i8(ISHFT(m,BIT_SIZE(m)),m3,
     $     'ISHFT(integer(8),integer(8)) 2')
      call c_i8(ISHFT(m,-BIT_SIZE(m)),m3,
     $     'ISHFT(integer(8),integer(8)) 3')
      call c_i8(ISHFT(m,0),m,'ISHFT(integer(8),integer(8)) 4')

c     ISHFTC - Section 13.13.50
c     The third argument is not optional in g77
      i  = 3
      i2 = 2
      i3 = 3
      ia = 5
      j  = 3
      j2 = 2
      j3 = 3
      ja = 5
      k  = 3
      k2 = 2
      k3 = 3
      ka = 5
      m2 = 2
      m3 = 3
      ma = 5
c     test all the combinations of arguments
      call c_i(ISHFTC(i,i2,i3),5,'ISHFTC(integer,integer,integer)')
      call c_i(ISHFTC(i,i2,j3),5,'ISHFTC(integer,integer,integer(2))')
      call c_i(ISHFTC(i,i2,k3),5,'ISHFTC(integer,integer,integer(1))')
      call c_i(ISHFTC(i,i2,m3),5,'ISHFTC(integer,integer,integer(8))')
      call c_i(ISHFTC(i,j2,i3),5,'ISHFTC(integer,integer(2),integer)')
      call c_i(ISHFTC(i,j2,j3),5,
     &  'ISHFTC(integer,integer(2),integer(2))')
      call c_i(ISHFTC(i,j2,k3),5,
     &  'ISHFTC(integer,integer(2),integer(1))')
      call c_i(ISHFTC(i,j2,m3),5,
     &  'ISHFTC(integer,integer(2),integer(8))')
      call c_i(ISHFTC(i,k2,i3),5,'ISHFTC(integer,integer(1),integer)')
      call c_i(ISHFTC(i,k2,j3),5,
     &  'ISHFTC(integer,integer(1),integer(2))')
      call c_i(ISHFTC(i,k2,k3),5,
     &  'ISHFTC(integer,integer(1),integer(1))')
      call c_i(ISHFTC(i,k2,m3),5,
     &  'ISHFTC(integer,integer(1),integer(8))')
      call c_i(ISHFTC(i,m2,i3),5,'ISHFTC(integer,integer(8),integer)')
      call c_i(ISHFTC(i,m2,j3),5,
     &  'ISHFTC(integer,integer(8),integer(2))')
      call c_i(ISHFTC(i,m2,k3),5,
     &  'ISHFTC(integer,integer(8),integer(1))')
      call c_i(ISHFTC(i,m2,m3),5,
     &  'ISHFTC(integer,integer(8),integer(8))')

      call c_i2(ISHFTC(j,i2,i3),ja,'ISHFTC(integer(2),integer,integer)')
      call c_i2(ISHFTC(j,i2,j3),ja,
     $     'ISHFTC(integer(2),integer,integer(2))')
      call c_i2(ISHFTC(j,i2,k3),ja,
     $     'ISHFTC(integer(2),integer,integer(1))')
      call c_i2(ISHFTC(j,i2,m3),ja,
     $     'ISHFTC(integer(2),integer,integer(8))')
      call c_i2(ISHFTC(j,j2,i3),ja,
     $     'ISHFTC(integer(2),integer(2),integer)')
      call c_i2(ISHFTC(j,j2,j3),ja,
     $     'ISHFTC(integer(2),integer(2),integer(2))')
      call c_i2(ISHFTC(j,j2,k3),ja,
     $     'ISHFTC(integer(2),integer(2),integer(1))')
      call c_i2(ISHFTC(j,j2,m3),ja,
     $     'ISHFTC(integer(2),integer(2),integer(8))')
      call c_i2(ISHFTC(j,k2,i3),ja,
     $     'ISHFTC(integer(2),integer(1),integer)')
      call c_i2(ISHFTC(j,k2,j3),ja,
     $     'ISHFTC(integer(2),integer(1),integer(2))')
      call c_i2(ISHFTC(j,k2,k3),ja,
     $     'ISHFTC(integer(2),integer(1),integer(1))')
      call c_i2(ISHFTC(j,k2,m3),ja,
     $     'ISHFTC(integer(2),integer(1),integer(8))')
      call c_i2(ISHFTC(j,m2,i3),ja,
     $     'ISHFTC(integer(2),integer(8),integer)')
      call c_i2(ISHFTC(j,m2,j3),ja,
     $     'ISHFTC(integer(2),integer(8),integer(2))')
      call c_i2(ISHFTC(j,m2,k3),ja,
     $     'ISHFTC(integer(2),integer(8),integer(1))')
      call c_i2(ISHFTC(j,m2,m3),ja,
     $     'ISHFTC(integer(2),integer(8),integer(8))')

      call c_i1(ISHFTC(k,i2,i3),ka,'ISHFTC(integer(1),integer,integer)')
      call c_i1(ISHFTC(k,i2,j3),ka,
     $     'ISHFTC(integer(1),integer,integer(2))')
      call c_i1(ISHFTC(k,i2,k3),ka,
     $     'ISHFTC(integer(1),integer,integer(1))')
      call c_i1(ISHFTC(k,i2,m3),ka,
     $     'ISHFTC(integer(1),integer,integer(8))')
      call c_i1(ISHFTC(k,j2,i3),ka,
     $     'ISHFTC(integer(1),integer(2),integer)')
      call c_i1(ISHFTC(k,j2,j3),ka,
     $     'ISHFTC(integer(1),integer(2),integer(2))')
      call c_i1(ISHFTC(k,j2,k3),ka,
     $     'ISHFTC(integer(1),integer(2),integer(1))')
      call c_i1(ISHFTC(k,j2,m3),ka,
     $     'ISHFTC(integer(1),integer(2),integer(8))')
      call c_i1(ISHFTC(k,k2,i3),ka,
     $     'ISHFTC(integer(1),integer(1),integer)')
      call c_i1(ISHFTC(k,k2,j3),ka,
     $     'ISHFTC(integer(1),integer(1),integer(2))')
      call c_i1(ISHFTC(k,k2,k3),ka,
     $     'ISHFTC(integer(1),integer(1),integer(1))')
      call c_i1(ISHFTC(k,k2,m3),ka,
     $     'ISHFTC(integer(1),integer(1),integer(8))')
      call c_i1(ISHFTC(k,m2,i3),ka,
     $     'ISHFTC(integer(1),integer(8),integer)')
      call c_i1(ISHFTC(k,m2,j3),ka,
     $     'ISHFTC(integer(1),integer(8),integer(2))')
      call c_i1(ISHFTC(k,m2,k3),ka,
     $     'ISHFTC(integer(1),integer(8),integer(1))')
      call c_i1(ISHFTC(k,m2,m3),ka,
     $     'ISHFTC(integer(1),integer(8),integer(8))')

      call c_i8(ISHFTC(m,i2,i3),ma,'ISHFTC(integer(8),integer,integer)')
      call c_i8(ISHFTC(m,i2,j3),ma,
     $     'ISHFTC(integer(8),integer,integer(2))')
      call c_i8(ISHFTC(m,i2,k3),ma,
     $     'ISHFTC(integer(8),integer,integer(1))')
      call c_i8(ISHFTC(m,i2,m3),ma,
     $     'ISHFTC(integer(8),integer,integer(8))')
      call c_i8(ISHFTC(m,j2,i3),ma,
     $     'ISHFTC(integer(8),integer(2),integer)')
      call c_i8(ISHFTC(m,j2,j3),ma,
     $     'ISHFTC(integer(8),integer(2),integer(2))')
      call c_i8(ISHFTC(m,j2,k3),ma,
     $     'ISHFTC(integer(8),integer(2),integer(1))')
      call c_i8(ISHFTC(m,j2,m3),ma,
     $     'ISHFTC(integer(8),integer(2),integer(8))')
      call c_i8(ISHFTC(m,k2,i3),ma,
     $     'ISHFTC(integer(8),integer(1),integer)')
      call c_i8(ISHFTC(m,k2,j3),ma,
     $     'ISHFTC(integer(1),integer(8),integer(2))')
      call c_i8(ISHFTC(m,k2,k3),ma,
     $     'ISHFTC(integer(1),integer(8),integer(1))')
      call c_i8(ISHFTC(m,k2,m3),ma,
     $     'ISHFTC(integer(1),integer(8),integer(8))')
      call c_i8(ISHFTC(m,m2,i3),ma,
     $     'ISHFTC(integer(8),integer(8),integer)')
      call c_i8(ISHFTC(m,m2,j3),ma,
     $     'ISHFTC(integer(8),integer(8),integer(2))')
      call c_i8(ISHFTC(m,m2,k3),ma,
     $     'ISHFTC(integer(8),integer(8),integer(1))')
      call c_i8(ISHFTC(m,m2,m3),ma,
     $     'ISHFTC(integer(8),integer(8),integer(8))')

c     test the corner cases
      call c_i(ISHFTC(i,BIT_SIZE(i),BIT_SIZE(i)),i,
     $     'ISHFTC(i,BIT_SIZE(i),BIT_SIZE(i)) i = integer')
      call c_i(ISHFTC(i,0,BIT_SIZE(i)),i,
     $     'ISHFTC(i,0,BIT_SIZE(i)) i = integer')
      call c_i(ISHFTC(i,-BIT_SIZE(i),BIT_SIZE(i)),i,
     $     'ISHFTC(i,-BIT_SIZE(i),BIT_SIZE(i)) i = integer')
      call c_i2(ISHFTC(j,BIT_SIZE(j),BIT_SIZE(j)),j,
     $     'ISHFTC(j,BIT_SIZE(j),BIT_SIZE(j)) j = integer(2)')
      call c_i2(ISHFTC(j,0,BIT_SIZE(j)),j,
     $     'ISHFTC(j,0,BIT_SIZE(j)) j = integer(2)')
      call c_i2(ISHFTC(j,-BIT_SIZE(j),BIT_SIZE(j)),j,
     $     'ISHFTC(j,-BIT_SIZE(j),BIT_SIZE(j)) j = integer(2)')
      call c_i1(ISHFTC(k,BIT_SIZE(k),BIT_SIZE(k)),k,
     $     'ISHFTC(k,BIT_SIZE(k),BIT_SIZE(k)) k = integer(1)')
      call c_i1(ISHFTC(k,0,BIT_SIZE(k)),k,
     $     'ISHFTC(k,0,BIT_SIZE(k)) k = integer(1)')
      call c_i1(ISHFTC(k,-BIT_SIZE(k),BIT_SIZE(k)),k,
     $     'ISHFTC(k,-BIT_SIZE(k),BIT_SIZE(k)) k = integer(1)')
      call c_i8(ISHFTC(m,BIT_SIZE(m),BIT_SIZE(m)),m,
     $     'ISHFTC(m,BIT_SIZE(m),BIT_SIZE(m)) m = integer(8)')
      call c_i8(ISHFTC(m,0,BIT_SIZE(m)),m,
     $     'ISHFTC(m,0,BIT_SIZE(m)) m = integer(8)')
      call c_i8(ISHFTC(m,-BIT_SIZE(m),BIT_SIZE(m)),m,
     $     'ISHFTC(m,-BIT_SIZE(m),BIT_SIZE(m)) m = integer(8)')

c     MVBITS - Section 13.13.74
      i = 6
      call MVBITS(7,2,2,i,0)
      call c_i(i,5,'MVBITS 1')
      j = 6
      j2 = 7
      ja = 5
      call MVBITS(j2,2,2,j,0)
      call c_i2(j,ja,'MVBITS 2')
      k = 6
      k2 = 7
      ka = 5
      call MVBITS(k2,2,2,k,0)
      call c_i1(k,ka,'MVBITS 3')
      m = 6
      m2 = 7
      ma = 5
      call MVBITS(m2,2,2,m,0)
      call c_i8(m,ma,'MVBITS 4')

c     NOT    - Section 13.13.77
c     Rather than assume integer sizes, mask off high bits
      j  = 21
      j2 = 31
      ja = 10
      k  = 21
      k2 = 31
      ka = 10
      m  = 21
      m2 = 31
      ma = 10
      call c_i(IAND(NOT(21),31),10,'NOT(integer)')
      call c_i2(IAND(NOT(j),j2),ja,'NOT(integer(2))')
      call c_i1(IAND(NOT(k),k2),ka,'NOT(integer(1))')
      call c_i8(IAND(NOT(m),m2),ma,'NOT(integer(8))')

      if ( fail ) STOP 1
      end

      subroutine failure(label)
c     Report failure and set flag
      character*(*) label
      logical fail
      common /flags/ fail
      write(6,'(a,a,a)') 'Test ',label,' FAILED'
      fail = .true.
      end

      subroutine c_l(i,j,label)
c     Check if LOGICAL i equals j, and fail otherwise
      logical i,j
      character*(*) label
      if ( i .eqv. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

      subroutine c_i(i,j,label)
c     Check if INTEGER i equals j, and fail otherwise
      integer i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

      subroutine c_i2(i,j,label)
c     Check if INTEGER(kind=2) i equals j, and fail otherwise
      integer(kind=2) i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

      subroutine c_i1(i,j,label)
c     Check if INTEGER(kind=1) i equals j, and fail otherwise
      integer(kind=1) i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

      subroutine c_i8(i,j,label)
c     Check if INTEGER(kind=8) i equals j, and fail otherwise
      integer(kind=8) i,j
      character*(*) label
      if ( i .ne. j ) then
         call failure(label)
         write(6,*) 'Got ',i,' expected ', j
      end if
      end

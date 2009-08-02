c { dg-do run }
c { dg-options "-std=legacy" }
c
      program short

      parameter   (   N=2  )
      common /chb/    pi,sig(0:N)
      common /parm/   h(2,2)

c  initialize some variables
      h(2,2) = 1117
      h(2,1) = 1178
      h(1,2) = 1568
      h(1,1) = 1621
      sig(0) = -1.
      sig(1) = 0.
      sig(2) = 1.

      call printout
      stop
      end

c ******************************************************************

      subroutine printout
      parameter   (   N=2  )
      common /chb/    pi,sig(0:N)
      common /parm/   h(2,2)
      dimension       yzin1(0:N), yzin2(0:N)

c  function subprograms
      z(i,j,k) = 0.5*h(i,j)*(sig(k)-1.)

c  a four-way average of rhobar
      do 260  k=0,N
        yzin1(k) = 0.25 * 
     &       ( z(2,2,k) + z(1,2,k) +
     &         z(2,1,k) + z(1,1,k) )
  260       continue

c  another four-way average of rhobar
      do 270  k=0,N
         rtmp1 = z(2,2,k)
         rtmp2 = z(1,2,k)
         rtmp3 = z(2,1,k)
         rtmp4 = z(1,1,k)
         yzin2(k) = 0.25 * 
     &       ( rtmp1 + rtmp2 + rtmp3 + rtmp4 )
  270       continue

      do k=0,N
         if (yzin1(k) .ne. yzin2(k)) call abort
      enddo
      if (yzin1(0) .ne. -1371.) call abort
      if (yzin1(1) .ne. -685.5) call abort
      if (yzin1(2) .ne. 0.) call abort

      return
      end


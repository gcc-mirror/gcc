c      PR optimization/13037
c      Contributed by Kirill Smelkov
c      bug symptom: zeta(kkzc) seems to reference to zeta(kkzc-1) instead
c      with gcc-3.2.2 it is OK, so it is a regression.
c
      subroutine bug1(expnt)
      implicit none

      double precision zeta
      common /bug1_area/zeta(3)

      double precision expnt(3)


      integer k, kkzc

      kkzc=0
      do k=1,3
         kkzc = kkzc + 1
         zeta(kkzc) = expnt(k)
      enddo

c     the following line activates the bug
      call bug1_activator(kkzc)
      end


c     dummy subroutine
      subroutine bug1_activator(inum)
      implicit none
      integer inum
      end


c     test driver
      program test_bug1
      implicit none

      double precision zeta
      common /bug1_area/zeta(3)

      double precision expnt(3)

      zeta(1) = 0.0d0
      zeta(2) = 0.0d0
      zeta(3) = 0.0d0

      expnt(1) = 1.0d0
      expnt(2) = 2.0d0
      expnt(3) = 3.0d0

      call bug1(expnt)
      if ((zeta(1).ne.1) .or. (zeta(2).ne.2) .or. (zeta(3).ne.3)) then
        call abort
      endif

      end


C { dg-do compile }
C { dg-options "-std=legacy -O3 -floop-interchange" }
C PR 50439 - this used to hang. Test case by Pat Haugen.

      subroutine comnul
C-----------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      parameter(zero=0.0d0,half=0.5d0,one=1.0d0)
      common/secom/rtc(9,18,10,5),rts(9,18,10,5)
      save
C-----------------------------------------------------------------------
      do 110 i1=1,9
        do 110 i2=1,18
          do 110 i3=1,10
            do 110 i4=1,5
              rtc(i1,i2,i3,i4)=zero
              rts(i1,i2,i3,i4)=zero
  110 continue
      return
      end

C PR traget/40587
C { dg-do compile }
C { dg-options "-O2" }
      subroutine TEST(i, r, result)
      implicit none
      integer i
      REAL*8 r
      REAL*8 result
      REAL*8 r2
      if(i.eq.0) then
         r2 = r
      else
         call ERROR()
      endif
      result = r2
      return
      end

! { dg-do compile }
! Tests the fix for PR44353
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>
!
      subroutine sub(i)
      intent(in) i
      integer ii(10)
      data (ii(i),i=1,10) /10*0/ ! failed here
      end

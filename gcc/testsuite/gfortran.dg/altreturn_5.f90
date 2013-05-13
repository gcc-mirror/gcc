! { dg-do run }
! { dg-options "-std=gnu" }
!
! Tests the fix for PR31483, in which dummy argument procedures
! produced an ICE if they had an alternate return.
!
! Contributed by Mathias Fröhlich <M.Froehlich@science-computing.de>

      SUBROUTINE R (i, *, *)
      INTEGER i
      RETURN i
      END

      SUBROUTINE PHLOAD (READER, i, res)
      IMPLICIT NONE
      EXTERNAL         READER
      integer i
      character(3) res
      CALL READER (i, *1, *2)
 1    res = "one"
      return
 2    res = "two"
      return
      END

      EXTERNAL R
      character(3) res
      call PHLOAD (R, 1, res)
      if (res .ne. "one") call abort ()
      CALL PHLOAD (R, 2, res)
      if (res .ne. "two") call abort ()
      END

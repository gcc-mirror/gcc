c { dg-do compile }
* Test case Toon submitted, cut down to expose the one bug.
* Belongs in compile/.
      SUBROUTINE INIERS1
      IMPLICIT   LOGICAL(L)
      COMMON/COMIOD/ NHIERS1, LERS1
      inquire(nhiers1, exist=lers1)
      END

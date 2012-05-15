! { dg-do compile }
! Tests the fix for PR28630, in which a contained,
! derived type function caused an ICE if its definition
! was both host and use associated.
!
! Contributed by Mark Hesselink <mhesseli@alumni.caltech.edu>
!
MODULE types
   TYPE :: t
      INTEGER :: i
   END TYPE
END MODULE types

MODULE foo
   USE types
CONTAINS
   FUNCTION bar (x) RESULT(r)
      USE types
      REAL, INTENT(IN) :: x
      TYPE(t) :: r
      r = t(0)
   END FUNCTION bar
END MODULE


LOGICAL FUNCTION foobar (x)
   USE foo
   REAL, INTENT(IN) :: x
   TYPE(t) :: c
   foobar = .FALSE.
   c = bar (x)
END FUNCTION foobar

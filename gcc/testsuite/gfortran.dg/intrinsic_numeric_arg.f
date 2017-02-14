! this test checks for a non-numeric argument to an
! intrinsic function (of which ABS() is one of many).
!      { dg-do compile }
       LOGICAL Z
       CHARACTER A
       REAL R
       R = ABS(Z) !  { dg-error " must have a numeric type" }
       R = ABS(A) !  { dg-error " must have a numeric type" }
       END

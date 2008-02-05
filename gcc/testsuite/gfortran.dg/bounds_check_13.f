! { dg-do compile }
! Tests the fix for PR34945, in which the lbound = KIND(YDA) was not resolved
! in time to set the size of TEST_ARRAY to zero.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
      SUBROUTINE VF0009(IDA1,IDA2,YDA,HDA)
      INTEGER(4) IDA1(4)
      INTEGER(4) IDA2(4)
      COMPLEX(8) YDA(2)
      INTEGER(4) HDA(3)
!  I N I T I A L I Z A T I O N  S E C T I O N
      COMPLEX(KIND=4) :: TEST_ARRAY
     $(  4:5,
     $   KIND(YDA):5,
     $   4:5,
     $   4:5  )
!  T E S T  S T A T E M E N T S
       IDA1(1:4) = LBOUND(TEST_ARRAY)
      END SUBROUTINE


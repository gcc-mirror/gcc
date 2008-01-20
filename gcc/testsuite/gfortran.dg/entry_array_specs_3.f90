! { dg-do compile }
!
! PR fortran/34861, in which the test of conformity of the result array bounds
! would barf because they are not known at compile time in this case.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
FUNCTION I_IMFUD0 ( IDA2 , NDS4, NDS3) RESULT(I_IMFUDP)
  INTEGER  ::   NDS4, NDS3
  INTEGER  ::   IDA2(5,NDS4,NDS3,2)
  INTEGER  ::   I_IMFUDP(SIZE(IDA2,1), SIZE(IDA2,2), SIZE(IDA2,3), SIZE(IDA2,4))
  ENTRY I_IMFUDX (NDS4, NDS3, IDA2) RESULT(I_IMFUDP)
  ENTRY I_IMFUDY (NDS3, NDS4, IDA2) RESULT(I_IMFUDP)
  ENTRY I_IMFUDZ (NDS3, IDA2, NDS4) RESULT(I_IMFUDP)
  I_IMFUDP = 1-IDA2(:,:,:,::NDS4-NDS3)
END FUNCTION

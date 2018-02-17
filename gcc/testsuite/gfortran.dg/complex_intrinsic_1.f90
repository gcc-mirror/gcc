! Testcase for the COMPLEX intrinsic
! { dg-do run }
  if (complex(1_1, -1_2) /= complex(1.0_4, -1.0_8)) STOP 1
  if (complex(1_4, -1.0) /= complex(1.0_4, -1_8)) STOP 2
  end

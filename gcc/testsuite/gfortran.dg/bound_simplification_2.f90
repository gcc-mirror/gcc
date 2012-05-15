! { dg-do compile }
! PR 38914 - this used to give an ICE due to missing
! simplification.
module foo
  INTEGER, PARAMETER, DIMENSION(0:20,4) :: IP_ARRAY2_4_S = 0
  INTEGER, PARAMETER, DIMENSION(2) ::  IP_ARRAY1_32_S = &
  & (/  LBOUND(IP_ARRAY2_4_S(5:10,2:3))/)
END module foo

! { dg-do run }
! { dg-options "-finit-local-zero -fbackslash" }

program init_flag_1
  call real_test
  call logical_test
  call int_test
  call complex_test
  call char_test
end program init_flag_1

! Test some initializations for both implicitly and
! explicitly declared local variables.
subroutine real_test
  real r1
  real r2(10)
  dimension r3(10,10)
  if (r1 /= 0.0) STOP 1
  if (r2(2) /= 0.0) STOP 2
  if (r3(5,5) /= 0.0) STOP 3
  if (r4 /= 0.0) STOP 4
end subroutine real_test

subroutine logical_test
  logical l1
  logical l2(2)
  if (l1 .neqv. .false.) STOP 5
  if (l2(2) .neqv. .false.) STOP 6
end subroutine logical_test

subroutine int_test
  integer i1
  integer i2(10)
  dimension i3(10,10)
  if (i1 /= 0) STOP 7
  if (i2(2) /= 0) STOP 8
  if (i3(5,5) /= 0) STOP 9
  if (i4 /= 0) STOP 10
end subroutine int_test

subroutine complex_test
  complex c1
  complex c2(20,20)
  if (c1 /= (0.0,0.0)) STOP 11
  if (c2(1,1) /= (0.0,0.0)) STOP 12
end subroutine complex_test

subroutine char_test
  character*1 c1
  character*8 c2, c3(5)
  character c4(10)
  if (c1 /= '\0') STOP 13
  if (c2 /= '\0\0\0\0\0\0\0\0') STOP 14
  if (c3(1) /= '\0\0\0\0\0\0\0\0') STOP 15
  if (c3(5) /= '\0\0\0\0\0\0\0\0') STOP 16
  if (c4(5) /= '\0') STOP 17
end subroutine char_test

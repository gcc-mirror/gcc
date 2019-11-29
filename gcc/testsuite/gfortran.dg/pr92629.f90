! { dg-do run }
! { dg-options "-fno-range-check" }
!
! Test the fix for PR92629.
program bge_tests
  if (bge (huge (1_1), 128_1)) stop 1
  if (bge (    128_1 , 255_1)) stop 2
  if (bge (huge (1_2), 32768_2)) stop 3
  if (bge (huge (1_4), 2147483648_4)) stop 4
  if (bge (huge (1_8), 9223372036854775808_8)) stop 5
end program

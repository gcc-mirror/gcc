/* Test the __arm_[r,w]sr ACLE intrinsics family.  */
/* Ensure correctness of the system register encoding parser.  */

/* { dg-do compile } */
/* { dg-options "-std=c2x -O3 -march=armv8-a" } */

#include <arm_acle.h>

void
test_leading_zeros ()
{
  __uint64_t b = __arm_rsr64 ("S1_2_C03_C04_5"); /* { dg-error "invalid system register name 's1_2_c03_c04_5'" } */
  __arm_wsr64 ("S1_2_C03_C04_5", b); /* { dg-error "invalid system register name 's1_2_c03_c04_5'" } */
}

void
test_bounds ()
{
  __uint64_t b;
  b = __arm_rsr64 ("s4_2_c3_c4_5"); /* { dg-error "invalid system register name 's4_2_c3_c4_5'" } */
  b = __arm_rsr64 ("s1_8_c3_c4_5"); /* { dg-error "invalid system register name 's1_8_c3_c4_5'" } */
  b = __arm_rsr64 ("s1_2_c16_c4_5"); /* { dg-error "invalid system register name 's1_2_c16_c4_5'" } */
  b = __arm_rsr64 ("s1_2_c3_c16_5"); /* { dg-error "invalid system register name 's1_2_c3_c16_5'" } */
  b = __arm_rsr64 ("s1_2_c3_c4_8"); /* { dg-error "invalid system register name 's1_2_c3_c4_8'" } */
}

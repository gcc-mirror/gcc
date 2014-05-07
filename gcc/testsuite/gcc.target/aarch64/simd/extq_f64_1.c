/* Test the `vextq_f64' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"
extern void abort (void);
#include <stdio.h>

float64x2_t
test_vextq_f64_1 (float64x2_t a, float64x2_t b)
{
  return vextq_f64 (a, b, 1);
}

int
main (int argc, char **argv)
{
  int i, off;
  float64_t arr1[] = {0, 1};
  float64x2_t in1 = vld1q_f64 (arr1);
  float64_t arr2[] = {2, 3};
  float64x2_t in2 = vld1q_f64 (arr2);
  float64_t exp[] = {1, 2};
  float64x2_t expected = vld1q_f64 (exp);
  float64x2_t actual = test_vextq_f64_1 (in1, in2);

  for (i = 0; i < 2; i++)
    if (actual[i] != expected[i])
      abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "ext\[ \t\]+\[vV\]\[0-9\]+\.16\[bB\], ?\[vV\]\[0-9\]+\.16\[bB\], ?\[vV\]\[0-9\]+\.16\[bB\], ?#\[0-9\]+\(?:.8\)?\(?:\[ \t\]+@\[a-zA-Z0-9 \]+\)?\n" 1 } } */
/* { dg-final { cleanup-saved-temps } } */

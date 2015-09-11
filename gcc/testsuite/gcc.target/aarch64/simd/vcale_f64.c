/* Test the vcale_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

#define SIZE 6

extern void abort (void);

volatile float64_t in[SIZE] = { -10.4, -3.14, 0.0, 1.5, 5.3, 532.3 };

int
main (void)
{
  uint64_t expected;
  uint64_t actual;
  float64x1_t arg1, arg2;
  int i, j;

  for (i = 0; i < SIZE; ++i)
   for (j = 0; j < SIZE; ++j)
     {
        expected = __builtin_fabs (in[i]) <= __builtin_fabs (in[j]) ? -1 : 0;
        arg1 = (float64x1_t) { in[i] };
        arg2 = (float64x1_t) { in[j] };
        actual = vget_lane_u64 (vcale_f64 (arg1, arg2), 0);

        if (actual != expected)
          abort ();
     }

  return 0;
}

/* { dg-final { scan-assembler "facge\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" } } */

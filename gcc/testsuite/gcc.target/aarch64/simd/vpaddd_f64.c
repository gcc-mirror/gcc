/* Test the vpaddd_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

#define SIZE 6

extern void abort (void);

float64_t in[SIZE] = { -4.0, 4.0, -2.0, 2.0, -1.0, 1.0 };

int
main (void)
{
  int i;

  for (i = 0; i < SIZE / 2; ++i)
    if (vpaddd_f64 (vld1q_f64 (in + 2 * i)) != 0.0)
      abort ();

  return 0;
}

/* { dg-final { scan-assembler "faddp\[ \t\]+\[dD\]\[0-9\]+, v\[0-9\].2d+\n" } } */

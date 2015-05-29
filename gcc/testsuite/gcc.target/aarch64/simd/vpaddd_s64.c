/* Test the vpaddd_s64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

#define SIZE 6

extern void abort (void);

int64_t in[SIZE] = { -4l, 4l, -2l, 2l, -1l, 1l };

int
main (void)
{
  int i;

  for (i = 0; i < SIZE / 2; ++i)
    if (vpaddd_s64 (vld1q_s64 (in + 2 * i)) != 0)
      abort ();

  return 0;
}

/* { dg-final { scan-assembler "addp\[ \t\]+\[dD\]\[0-9\]+, v\[0-9\].2d+\n" } } */

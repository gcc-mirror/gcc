/* Test the vpaddd_u64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

#define SIZE 6

extern void abort (void);

uint64_t in[SIZE] = { 4ul, 4ul, 2ul, 2ul, 1ul, 1ul };

int
main (void)
{
  int i;

  for (i = 0; i < SIZE / 2; ++i)
    if (vpaddd_u64 (vld1q_u64 (in + 2 * i)) != 2 * in[2 * i])
      abort ();

  return 0;
}

/* { dg-final { scan-assembler "addp\[ \t\]+\[dD\]\[0-9\]+, v\[0-9\]+.2d+\n" } } */

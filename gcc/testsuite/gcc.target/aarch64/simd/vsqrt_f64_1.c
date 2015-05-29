/* Test the vsqrt_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"

extern void abort (void);


int
main (void)
{
  volatile float64x1_t in = vcreate_f64(0x3febd3e560634d7bULL);
  float64x1_t result = vsqrt_f64 (in);
  float64_t expected = 0.9325321502142351;

  if (result[0] != expected)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler "fsqrt\[ \t\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+\n" } } */

/* Test the `vextf64' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

extern void abort (void);

int
main (int argc, char **argv)
{
  int i, off;
  float64x1_t in1 = {0};
  float64x1_t in2 = {1};
  float64x1_t actual = vext_f64 (in1, in2, 0);
  if (actual[0] != in1[0])
    abort ();

  return 0;
}

/* Do not scan-assembler.  An EXT instruction could be emitted, but would merely
   return its first argument, so it is legitimate to optimize it out.  */

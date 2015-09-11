/* Test the `vextu64' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

extern void abort (void);

int
main (int argc, char **argv)
{
  uint64_t arr1[] = {0};
  uint64x1_t in1 = vld1_u64 (arr1);
  uint64_t arr2[] = {1};
  uint64x1_t in2 = vld1_u64 (arr2);
  uint64x1_t actual = vext_u64 (in1, in2, 0);
  if (actual[0] != in1[0])
    abort ();

  return 0;
}

/* Do not scan-assembler.  An EXT instruction could be emitted, but would merely
   return its first argument, so it is legitimate to optimize it out.  */

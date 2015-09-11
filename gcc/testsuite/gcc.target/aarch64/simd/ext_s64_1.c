/* Test the `vexts64' AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

extern void abort (void);

int
main (int argc, char **argv)
{
  int64_t arr1[] = {0};
  int64x1_t in1 = vld1_s64 (arr1);
  int64_t arr2[] = {1};
  int64x1_t in2 = vld1_s64 (arr2);
  int64x1_t actual = vext_s64 (in1, in2, 0);
  if (actual[0] != in1[0])
    abort ();

  return 0;
}

/* Do not scan-assembler.  An EXT instruction could be emitted, but would merely
   return its first argument, so it is legitimate to optimize it out.  */

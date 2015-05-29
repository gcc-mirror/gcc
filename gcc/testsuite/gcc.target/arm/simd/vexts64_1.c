/* Test the `vexts64' ARM Neon intrinsic.  */

/* { dg-options "-save-temps -O3 -fno-inline" } */
/* { dg-add-options arm_neon } */

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
  if (actual != in1)
    abort ();

  return 0;
}

/* Don't scan assembler for vext - it can be optimized into a move from r0.  */

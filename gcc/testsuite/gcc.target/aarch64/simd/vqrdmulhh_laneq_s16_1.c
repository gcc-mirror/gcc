/* Test the vqrdmulhh_laneq_s16 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3 -fno-inline" } */

#include "arm_neon.h"

extern void abort (void);

int
main (void)
{
  int16_t arg1;
  int16x8_t arg2;
  int16_t actual;
  int16_t expected;

  arg1 = 0;
  arg2 = vcombine_s16 (vcreate_s16 (0x7fffffffa7908000ULL),
                       vcreate_s16 (0x8000d2607fff0000ULL));

  actual = vqrdmulhh_laneq_s16 (arg1, arg2, 7);
  expected = 0;

  if (expected != actual)
    abort ();

  return 0;
}


/* { dg-final { scan-assembler-times "sqrdmulh\[ \t\]+\[hH\]\[0-9\]+, ?\[hH\]\[0-9\]+, ?\[vV\]\[0-9\]+\.\[hH\]\\\[7\\\]\n" 1 } } */

/* { dg-options "-O2" } */

#include <arm_neon.h>

float64x2x2_t
f1 (float64x2x2_t x)
{
  x.val[0][1] += 1.0;
  return x;
}

float64x2x3_t
f2 (float64x2x3_t x)
{
  x.val[0][0] = x.val[1][1] + x.val[2][0];
  return x;
}

float64x2x4_t
f3 (float64x2x4_t x)
{
  x.val[0][0] = x.val[1][1] + x.val[2][0] - x.val[3][1];
  return x;
}

/* { dg-final { scan-assembler-not {\tmov\t} } } */
/* { dg-final { scan-assembler-not {\[sp,} } } */

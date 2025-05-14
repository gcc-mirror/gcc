/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "arm_acle.h"

double test_acle_fma (double x, double y, double z)
{
  return __fma (x, y, z);
}

float test_acle_fmaf (float x, float y, float z)
{
  return __fmaf (x, y, z);
}

/* { dg-final { scan-assembler-times "fmadd\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fmadd\ts\[0-9\]" 1 } } */

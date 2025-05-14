/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "arm_acle.h"

double
test_acle_sqrt (double x)
{
  return __sqrt (x);
}

float
test_acle_sqrtf (float x)
{
  return __sqrtf (x);
}

/* { dg-final { scan-assembler-times "fsqrt\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "fsqrt\ts\[0-9\]" 1 } } */

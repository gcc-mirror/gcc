/* { dg-do run } */
/* { dg-options "-O2 -msm4" } */
/* { dg-require-effective-target sm4 } */

#include "sm4-check.h"

char rnds;
SM4_FUNC (rnds);

static void
sm4_test (void)
{
  SM4_AVX_SIMULATE (rnds);
}

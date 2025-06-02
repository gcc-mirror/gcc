/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -msm4 -mavx10.2" } */
/* { dg-require-effective-target sm4 } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#include "sm4-check.h"

char rnds;
SM4_FUNC (rnds);

static void
sm4_avx10_test (void)
{
  SM4_AVX10_SIMULATE (rnds);
}

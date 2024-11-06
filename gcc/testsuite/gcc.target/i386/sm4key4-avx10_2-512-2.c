/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -msm4 -mavx10.2-512" } */
/* { dg-require-effective-target sm4 } */
/* { dg-require-effective-target avx10_2_512 } */

#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#include "sm4-check.h"

char key;
SM4_FUNC (key);

static void
sm4_avx512f_test (void)
{
  SM4_AVX512F_SIMULATE (key);
}

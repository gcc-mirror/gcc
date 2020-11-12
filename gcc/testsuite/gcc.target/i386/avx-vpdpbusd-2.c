/* { dg-do run } */
/* { dg-options "-O2 -mavxvnni" } */
/* { dg-require-effective-target avxvnni } */

#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK

static void
CALC (int *r, int *dst, unsigned char *s1, char *s2, int size)
{
  short tempres[32];
  for (int i = 0; i < size; i++) {
    tempres[i] = ((unsigned short)(s1[i]) * (short)(s2[i]));
  }
  for (int i = 0; i < size / 4; i++) {
    long long test = (long long)dst[i] + tempres[i*4] + tempres[i*4 + 1] + tempres[i*4 + 2] + tempres[i*4 + 3];
    r[i] = test;
  }
}

void
TEST (void)
{
  int i;
  union256i_d res_256;
  union256i_b src2_256;
  union256i_ub src1_256;
  int res_ref_256[8];

  if (!__builtin_cpu_supports ("avxvnni"))
    return;

  for (i = 0; i < 32; i++)
    {
      int sign = i % 2 ? 1 : -1;
      src1_256.a[i] = 10 + 3*i + sign;
      src2_256.a[i] = sign*10*i*i;
    }

  for (i = 0; i < 8; i++)
    res_256.a[i] = 0x7fffffff;

  CALC (res_ref_256, res_256.a, src1_256.a, src2_256.a, 32);
  res_256.x = _mm256_dpbusd_avx_epi32 (res_256.x, src1_256.x, src2_256.x);
  if (check_union256i_d (res_256, res_ref_256))
    abort ();

  union128i_d res_128;
  union128i_b src2_128;
  union128i_ub src1_128;
  int res_ref_128[4];

  for (i = 0; i < 16; i++)
    {
      int sign = i % 2 ? 1 : -1;
      src1_128.a[i] = 10 + 3*i*i + sign;
      src2_128.a[i] = sign*10*i*i;
    }

  for (i = 0; i < 4; i++)
    res_128.a[i] = 0x7fffffff;

  CALC (res_ref_128, res_128.a, src1_128.a, src2_128.a, 16);
  res_128.x = _mm_dpbusd_avx_epi32 (res_128.x, src1_128.x, src2_128.x);
  if (check_union128i_d (res_128, res_ref_128))
    abort ();
}

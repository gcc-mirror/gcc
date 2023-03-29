/* { dg-do run } */
/* { dg-options "-O2 -mavxifma" } */
/* { dg-require-effective-target avxifma } */
#define AVXIFMA
#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK

void
CALC (unsigned long long *r, unsigned long long *s1,
      unsigned long long *s2, unsigned long long *s3,
      int size)
{
  int i;

  for (i = 0; i < size; i++)
    {
      r[i] = s2[i] * s3[i] + s1[i];
    }
}

void
TEST (void)
{
  union256i_q src1_256, src2_256, dst_256;
  union128i_q src1_128, src2_128, dst_128;
  unsigned long long dst_ref_256[4], dst_ref_128[2];
  int i;

  for (i = 0; i < 4; i++)
  {
    src1_256.a[i] = 3450 * i;
    src2_256.a[i] = 7863 * i;
    dst_256.a[i] = 117;
  }

 for (i = 0; i < 2; i++)
  {
    src1_128.a[i] = 3540 * i;
    src2_128.a[i] = 7683 * i;
    dst_128.a[i] = 117;
  }

  CALC (dst_ref_256, dst_256.a, src1_256.a, src2_256.a, 4);
  dst_256.x = _mm256_madd52lo_avx_epu64 (dst_256.x, src1_256.x, src2_256.x);
  if (check_union256i_q (dst_256, dst_ref_256))
    abort ();

  CALC (dst_ref_128, dst_128.a, src1_128.a, src2_128.a, 2);
  dst_128.x = _mm_madd52lo_avx_epu64 (dst_128.x, src1_128.x, src2_128.x);
  if (check_union128i_q (dst_128, dst_ref_128))
    abort ();

}


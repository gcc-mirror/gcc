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
CALC (long long *r, long long *s1, long long *s2, long long *s3, int size)
{
  int i;
  long long a,b;

  for (i = 0; i < size; i++)
    {
      /* Simulate higher 52 bits out of 104 bit,
	 by shifting opernads with 0 in lower 26 bits.  */
      a = s2[i] >> 26;
      b = s3[i] >> 26;
      r[i] = a * b + s1[i];
    }
}

void
TEST (void)
{
  union256i_q src1_256, src2_256, dst_256;
  union128i_q src1_128, src2_128, dst_128;
  long long dst_ref_256[4], dst_ref_128[2];
  int i;

  for (i = 0; i < 4; i++)
  {
    src1_256.a[i] = 15 + 3467 * i;
    src2_256.a[i] = 9217 + i;
    src1_256.a[i] = src1_256.a[i] << 26;
    src2_256.a[i] = src2_256.a[i] << 26;
    src1_256.a[i] &= ((1LL << 52) - 1);
    src2_256.a[i] &= ((1LL << 52) - 1);
    dst_256.a[i] = -1;
  }

 for (i = 0; i < 2; i++)
  {
    src1_128.a[i] = 16 + 3467 * i;
    src2_128.a[i] = 9127 + i;
    src1_128.a[i] = src1_128.a[i] << 26;
    src2_128.a[i] = src2_128.a[i] << 26;
    src1_128.a[i] &= ((1LL << 52) - 1);
    src2_128.a[i] &= ((1LL << 52) - 1);
    dst_128.a[i] = -1;
  }

  CALC (dst_ref_256, dst_256.a, src1_256.a, src2_256.a, 4);
  dst_256.x = _mm256_madd52hi_avx_epu64 (dst_256.x, src1_256.x, src2_256.x);
  if (check_union256i_q (dst_256, dst_ref_256))
    abort ();

  CALC (dst_ref_128, dst_128.a, src1_128.a, src2_128.a, 2);
  dst_128.x = _mm_madd52hi_avx_epu64 (dst_128.x, src1_128.x, src2_128.x);
  if (check_union128i_q (dst_128, dst_ref_128))
    abort ();

}


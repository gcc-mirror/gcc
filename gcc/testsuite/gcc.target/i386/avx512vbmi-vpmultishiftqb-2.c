/* { dg-do run } */
/* { dg-options "-O2 -mavx512vbmi" } */
/* { dg-require-effective-target avx512vbmi } */

#define AVX512VBMI

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (char *r, char *s1, char *s2)
{
  int i, j, k;
  long long a, b, ctrl;

  for (i = 0; i < SIZE / sizeof (long long); i++)
    {
      union
      {
	long long x;
	char a[sizeof(long long)];
      } src;

      for (j = 0; j  < sizeof (long long); j++)
	src.a[j] = s2[i * sizeof (long long) + j];
      for (j = 0; j  < sizeof (long long); j++)
	{
	  ctrl = s1[i * sizeof (long long) + j] & ((1 << sizeof (long long)) - 1);
	  r[i * sizeof (long long) + j] = 0;
	  for (k = 0; k < 8; k++)
	    {
	       r[i * sizeof (long long) + j] |= ((src.x >> ((ctrl + k) % (sizeof (long long) * 8))) & 1) << k;
	    }
	}
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_b) src1, src2, dst1, dst2, dst3;
  char dst_ref[SIZE];
  int i;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 15 + 3467 * i;
      src2.a[i] = 9217 + i;
      dst2.a[i] = DEFAULT_VALUE;
    }

  CALC (dst_ref, src1.a, src2.a);
  dst1.x = INTRINSIC (_multishift_epi64_epi8) (src1.x, src2.x);
  dst2.x = INTRINSIC (_mask_multishift_epi64_epi8) (dst2.x, mask, src1.x, src2.x);
  dst3.x = INTRINSIC (_maskz_multishift_epi64_epi8) (mask, src1.x, src2.x);

  if (UNION_CHECK (AVX512F_LEN, i_b) (dst1, dst_ref))
    abort ();

  MASK_MERGE (i_b) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (dst2, dst_ref))
    abort ();

  MASK_ZERO (i_b) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (dst3, dst_ref))
    abort ();
}

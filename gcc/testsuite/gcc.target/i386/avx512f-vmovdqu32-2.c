/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -Wno-address-of-packed-member" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE ((AVX512F_LEN) / 32)
#include "avx512f-mask-type.h"

typedef struct
{
  char c;
  int a[SIZE];
} __attribute__ ((packed)) EVAL(unaligned_array, AVX512F_LEN,);

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s2, res1, res3, res4;
  EVAL(unaligned_array, AVX512F_LEN,) s1, res2, res5;
  MASK_TYPE mask = MASK_VALUE;
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 12345 * (i + 2000) * sign;
      s2.a[i] = 67890 * (i + 2000) * sign;
      res3.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

#if AVX512F_LEN == 512
  res1.x = _mm512_loadu_si512 (s1.a);
  _mm512_storeu_si512 (res2.a, s2.x);
#endif
  res3.x = INTRINSIC (_mask_loadu_epi32) (res3.x, mask, s1.a);
  res4.x = INTRINSIC (_maskz_loadu_epi32) (mask, s1.a);
  INTRINSIC (_mask_storeu_epi32) (res5.a, mask, s2.x);

#if AVX512F_LEN == 512
  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, s1.a))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_d) (s2, res2.a))
    abort ();
#endif

  MASK_MERGE (i_d) (s1.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, s1.a))
    abort ();

  MASK_ZERO (i_d) (s1.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res4, s1.a))
    abort ();

  MASK_MERGE (i_d) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (s2, res5.a))
    abort ();
}

/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE ((AVX512F_LEN) / 64)
#include "avx512f-mask-type.h"

typedef struct
{
  char c;
  long long a[SIZE];
} __attribute__ ((packed)) EVAL(unaligned_array, AVX512F_LEN,);

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) s2, res1, res2;
  EVAL(unaligned_array, AVX512F_LEN,) s1, res3, res4;
  MASK_TYPE mask = MASK_VALUE;
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 12345 * (i + 2000) * sign;
      s2.a[i] = 67890 * (i + 2000) * sign;
      res1.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_mask_loadu_epi64) (res1.x, mask, s1.a);
  res2.x = INTRINSIC (_maskz_loadu_epi64) (mask, s1.a);
  INTRINSIC (_mask_storeu_epi64) (res3.a, mask, s2.x);
  INTRINSIC (_storeu_epi64) (res4.a, s2.x);

  MASK_MERGE (i_q) (s1.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, s1.a))
    abort ();

  MASK_ZERO (i_q) (s1.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, s1.a))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_q) (s2, res4.a))
    abort ();

  MASK_MERGE (i_q) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (s2, res3.a))
    abort ();
}

/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif

#include "avx10-helper.h"
#include "fp8-helper.h"

#define SIZE_SRC (AVX512F_LEN_HALF / 8)
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (_Float16 *r, unsigned char *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    r[i] = convert_hf8_to_fp16(s[i]);
}

void
TEST (void)
{
  int i,sign;
  UNION_TYPE (AVX512F_LEN, h) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN_HALF, i_b) src;
  MASK_TYPE mask = MASK_VALUE;
  _Float16 res_ref[SIZE];

  sign = 1;
  for (i = 0; i < SIZE_SRC; i++)
    {
      src.a[i] = sign * (2.5 * (1 << (i % 3)));
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  CALC(res_ref, src.a);

  res1.x = INTRINSIC (_cvthf8_ph) (src.x);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, h) (res1, res_ref, 0.0009765625))
    abort ();

  res2.x = INTRINSIC (_mask_cvthf8_ph) (res2.x, mask, src.x);
  MASK_MERGE (h) (res_ref, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, h) (res2, res_ref, 0.0009765625))
    abort ();

  res3.x = INTRINSIC (_maskz_cvthf8_ph) (mask, src.x);
  MASK_ZERO (h) (res_ref, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, h) (res3, res_ref, 0.0009765625))
    abort ();
}

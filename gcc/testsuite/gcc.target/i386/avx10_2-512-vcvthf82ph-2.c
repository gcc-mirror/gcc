/* { dg-do run } */
/* { dg-options "-O2 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif

#include "avx10-helper.h"
#include "fp8-helper.h"

#define SIZE_SRC (AVX512F_LEN_HALF / 8)
#define SIZE_RES (AVX512F_LEN / 16)

void
CALC (_Float16 *r, unsigned char *s)
{
  int i;
  for (i = 0; i < SIZE_RES; i++)
    r[i] = convert_hf8_to_fp16(s[i]);
}

void
TEST (void)
{
  int i,sign;
  UNION_TYPE (AVX512F_LEN, h) res;
  UNION_TYPE (AVX512F_LEN_HALF, i_b) src;
  _Float16 res_ref[SIZE_RES];

  sign = 1;
  for (i = 0; i < SIZE_SRC; i++)
    {
      src.a[i] = sign * (2.5 * (1 << (i % 3)));
      sign = -sign;
    }

  res.x = INTRINSIC (_cvthf8_ph) (src.x);
  CALC(res_ref, src.a);

  if (UNION_ROUGH_CHECK (AVX512F_LEN, h) (res, res_ref, 0.0009765625))
    abort ();
}

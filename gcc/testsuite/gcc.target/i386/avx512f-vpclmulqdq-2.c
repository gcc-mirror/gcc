/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mvpclmulqdq" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target vpclmulqdq } */

#define AVX512F

#define VPCLMULQDQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)

#include "avx512f-mask-type.h"

static void
CALC (unsigned long long *r, unsigned long long *s1, unsigned long long *s2, unsigned char imm)
{
  for (int len = 0; len < SIZE/2; len++)
  {
    unsigned long long src1, src2;
    src1 = (imm & 1) ? s1[len*2 + 1] : s1[len*2];
    src2 = ((imm >> 4) & 1) ? s2[len*2 + 1] : s2[len*2];
    for (int i = 0; i < 64; i++)
      {
        if ((src1 >> i) & 1)
          {
            if (i)
              r[len*2 + 1] ^= src2 >> (64 - i);
            r[len*2] ^= src2 << i;
          }
      }
  }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) res, src1, src2;
  unsigned long long res_ref[SIZE];
  unsigned char imm = 1;  

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 0xFFFFFFFFF + i;
      src2.a[i] = 0xFFFFFFFFF + i*i;
    }

  for (i = 0; i < SIZE; i++)
    {
      res.a[i] = 0;
      res_ref[i] = 0;
    }

  CALC (res_ref, src1.a, src2.a, imm);
  res.x = INTRINSIC (_clmulepi64_epi128) (src1.x, src2.x, imm);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res, res_ref))
    abort ();
}

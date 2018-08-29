/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mgfni -mavx512bw" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target gfni } */

#define AVX512F

#define GFNI
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)

#include "avx512f-mask-type.h"

static void
CALC (unsigned char *r, unsigned char *s1, unsigned char *s2)
{
  for (int i = 0; i < SIZE; i++)
    {
      unsigned short result = 0;
      for (int bit = 0; bit < 8; bit++)
        {
          if ((s1[i] >> bit) & 1)
          {
            result ^= s2[i] << bit;
          }
        }
       // Reduce result by x^8 + x^4 + x^3 + x + 1
       for (int bit = 14; bit > 7; bit--)
         {
           unsigned short p = 0x11B << (bit - 8);
           if ((result >> bit) & 1)
             result ^= p;
         }
       r[i] = result; 
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_b) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1 + i;
      src2.a[i] = 2 + 2*i;
    }

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, src1.a, src2.a);

  res1.x = INTRINSIC (_gf2p8mul_epi8) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_gf2p8mul_epi8) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_gf2p8mul_epi8) (mask, src1.x, src2.x);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}

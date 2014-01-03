/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) val;
  UNION_TYPE (AVX512F_LEN_HALF, i_w) res1,res2,res3;
  MASK_TYPE mask = MASK_VALUE;
  short exp[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  val.a[0] = 1;
  val.a[1] = 2;
  val.a[2] = 4;
  val.a[3] = 8;
#if AVX512F_LEN > 128
  val.a[4] = -1;
  val.a[5] = -2;
  val.a[6] = -4;
  val.a[7] = -8;
#endif
#if AVX512F_LEN > 256
  val.a[8] = 1;
  val.a[9] = 2;
  val.a[10] = 4;
  val.a[11] = 8;
  val.a[12] = -1;
  val.a[13] = -2;
  val.a[14] = -4;
  val.a[15] = -8;
#endif

  exp[0] = 0x3c00;
  exp[1] = 0x4000;
  exp[2] = 0x4400;
  exp[3] = 0x4800;
#if AVX512F_LEN > 128
  exp[4] = 0xbc00;
  exp[5] = 0xc000;
  exp[6] = 0xc400;
  exp[7] = 0xc800;
#endif
#if AVX512F_LEN > 256
  exp[8] = 0x3c00;
  exp[9] = 0x4000;
  exp[10] = 0x4400;
  exp[11] = 0x4800;
  exp[12] = 0xbc00;
  exp[13] = 0xc000;
  exp[14] = 0xc400;
  exp[15] = 0xc800;
#endif

  res1.x = _mm512_cvtps_ph (val.x, 0);
  res2.x = _mm512_mask_cvtps_ph (res2.x, mask, val.x, 0);
  res3.x = _mm512_maskz_cvtps_ph (mask, val.x, 0);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_w) (res1, exp))
    abort ();

  MASK_MERGE (i_w) (exp, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_w) (res2, exp))
    abort ();

  MASK_ZERO (i_w) (exp, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_w) (res3, exp))
    abort ();
}

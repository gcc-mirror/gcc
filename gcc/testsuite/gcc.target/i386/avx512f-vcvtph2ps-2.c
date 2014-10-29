/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN_HALF, i_w) val;
  UNION_TYPE (AVX512F_LEN,) res1,res2,res3;
  MASK_TYPE mask = MASK_VALUE;
  float exp[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  exp[0] = 1;
  exp[1] = 2;
  exp[2] = 4;
  exp[3] = 8;
#if AVX512F_LEN > 128
  exp[4] = -1;
  exp[5] = -2;
  exp[6] = -4;
  exp[7] = -8;
#endif
#if AVX512F_LEN > 256
  exp[8] = 1;
  exp[9] = 2;
  exp[10] = 4;
  exp[11] = 8;
  exp[12] = -1;
  exp[13] = -2;
  exp[14] = -4;
  exp[15] = -8;
#endif

  val.a[0] = 0x3c00;
  val.a[1] = 0x4000;
  val.a[2] = 0x4400;
  val.a[3] = 0x4800;
#if AVX512F_LEN > 128
  val.a[4] = 0xbc00;
  val.a[5] = 0xc000;
  val.a[6] = 0xc400;
  val.a[7] = 0xc800;
#endif
#if AVX512F_LEN > 256
  val.a[8] = 0x3c00;
  val.a[9] = 0x4000;
  val.a[10] = 0x4400;
  val.a[11] = 0x4800;
  val.a[12] = 0xbc00;
  val.a[13] = 0xc000;
  val.a[14] = 0xc400;
  val.a[15] = 0xc800;
#endif

  res1.x = INTRINSIC (_cvtph_ps) (val.x);
  res2.x = INTRINSIC (_mask_cvtph_ps) (res2.x, mask, val.x);
  res3.x = INTRINSIC (_maskz_cvtph_ps) (mask, val.x);

  if (UNION_CHECK (AVX512F_LEN,) (res1, exp))
    abort ();

  MASK_MERGE () (exp, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res2, exp))
    abort ();

  MASK_ZERO () (exp, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res3, exp))
    abort ();
}

/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mvaes" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target avx512vaes } */

#define AVX512F

#define VAES
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)

#include "avx512f-mask-type.h"

static void
CALC (unsigned int *r)
{
  for (int i = 0; i < SIZE; i+=4)
    {
      r[i] = 0xba0cda94;
      r[i + 1] = 0x73676a7;
      r[i + 2] = 0xd3204422;
      r[i + 3] = 0x5506edd;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_ud) res1, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned int res_ref[SIZE];

  for (int i = 0; i < SIZE; i+=4)
    {
      src1.a[i] = 0x5d53475d;
      src1.a[i + 1] = 0x63746f72;
      src1.a[i + 2] = 0x73745665;
      src1.a[i + 3] = 0x7b5b5465;
      src2.a[i] = 0x726f6e5d;
      src2.a[i + 1] = 0x5b477565;
      src2.a[i + 2] = 0x68617929;
      src2.a[i + 3] = 0x48692853;
    }

  CALC (res_ref);
  res1.x = INTRINSIC (_aesdec_epi128) (src2.x, src1.x);

  if (UNION_CHECK (AVX512F_LEN, i_ud) (res1, res_ref))
    abort ();
}

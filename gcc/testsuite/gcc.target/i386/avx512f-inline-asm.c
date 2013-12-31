/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

static void
init_vpadd_mask (int* dst, int *src1, int *src2, int seed)
{
  int i;

  for (i = 0; i < 16; i++)
    {
      dst[i] = -1;
      src1[i] = seed * 2 * i + 1;
      src2[i] = seed * 2 * i;
    }
}

static inline void
calc_vpadd_mask_zeroed (int *dst, __mmask16 m, int *src1, int *src2)
{
  int i;

  for (i = 0; i < 16; i++)
    {
      if (m & (1 << i))
	dst[i] = src1[i] + src2[i];
      else
	dst[i] = 0;
    }
}

void static
avx512f_test (void)
{
  /* Checking mask arithmetic instruction */

  __mmask16 msk_dst, msk_src1, msk_src2, msk_dst_ref;

  msk_src1 = 0x0FFB;
  msk_src2 = 0x0F0F;

  asm ("kandw\t%2, %1, %0"
       : "=Yk" (msk_dst)
       : "Yk" (msk_src1), "Yk" (msk_src2));

  msk_dst_ref =  _mm512_kand (msk_src1, msk_src2);
  if (msk_dst != msk_dst_ref)
    abort ();


  /* Checking zero-masked vector instruction */
  union512i_d dst, src1, src2;
  int dst_ref[16];

  init_vpadd_mask (dst.a,   src1.a, src2.a, 1);
  init_vpadd_mask (dst_ref, src1.a, src2.a, 1);

  asm ("vpaddd\t%2, %1, %0 %{%3%}%{z%}"
       : "=x" (dst.x)
       : "x" (src1.x), "x" (src2.x), "k" (msk_dst));

  calc_vpadd_mask_zeroed (dst_ref, msk_dst, src1.a, src2.a);

  if (check_union512i_d (dst, dst_ref))
    abort ();
}

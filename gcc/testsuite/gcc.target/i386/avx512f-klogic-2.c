/* { dg-do run } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

void
avx512f_test (void)
{
  __mmask16 dst, src1, src2, dst_ref;
  volatile __m512 x = _mm512_setzero_ps(); 

  __asm__( "kmovw %1, %0" : "=k" (src1) : "r" (0x0FFF) );
  __asm__( "kmovw %1, %0" : "=k" (src2) : "r" (0x0F0F) );

  dst = _mm512_kand (src1, src2);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = src1 & src2;
  if (dst != dst_ref)
    abort ();

  dst = _mm512_kandn (src1, src2);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = ~src1 & src2;
  if (dst != dst_ref)
    abort ();

  dst = _mm512_kor (src1, src2);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = src1 | src2;
  if (dst != dst_ref)
    abort ();

  dst = _mm512_kxnor (src1, src2);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = ~(src1 ^ src2);
  if (dst != dst_ref)
    abort ();

  dst = _mm512_kxor (src1, src2);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = src1 ^ src2;
  if (dst != dst_ref)
    abort ();

  dst = _mm512_knot (src1);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = ~src1;
  if (dst != dst_ref)
    abort ();

  dst = _mm512_kunpackb (src1, src2);
  x = _mm512_mask_add_ps (x, dst, x, x);
  dst_ref = ((src2 << 8) | src1) & 0xFFFF;

  if (dst != dst_ref)
    abort ();
}

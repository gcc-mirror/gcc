/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mx32 -O2 -mavx512pf" } */
/* { dg-final { scan-assembler-not "\tvgather" } } */
/* { dg-final { scan-assembler "addr32 vgather" } } */

typedef int __v8si __attribute__ ((__vector_size__ (32)));
typedef long long __m256i __attribute__ ((__vector_size__ (32),
					  __may_alias__));
typedef unsigned char  __mmask8;

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_prefetch_i32gather_pd (__m256i __index, void const *__addr,
			      int __scale, int __hint)
{
  __builtin_ia32_gatherpfdpd ((__mmask8) 0xFF, (__v8si) __index, __addr,
			      __scale, __hint);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_prefetch_i32gather_pd (__m256i __index, __mmask8 __mask,
				   void const *__addr, int __scale, int __hint)
{
  __builtin_ia32_gatherpfdpd (__mask, (__v8si) __index, __addr, __scale,
			      __hint);
}

volatile __m256i idx;
volatile __mmask8 m8;

void extern
avx512pf_test (void)
{
  _mm512_prefetch_i32gather_pd (idx, (void *) 0, 8, 3);
  _mm512_mask_prefetch_i32gather_pd (idx, m8, (void *) 0, 8, 3);
}

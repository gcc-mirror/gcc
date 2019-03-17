/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mx32 -O2 -mavx512f" } */
/* { dg-final { scan-assembler "\tvscatter" } } */
/* { dg-final { scan-assembler-not "addr32 vscatter" } } */

typedef long long __v8di __attribute__ ((__vector_size__ (64)));
typedef double __v8df __attribute__ ((__vector_size__ (64)));
typedef long long __m512i __attribute__ ((__vector_size__ (64), __may_alias__));
typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));
typedef unsigned char  __mmask8;

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_i64scatter_pd (void *__addr, __m512i __index, __m512d __v1,
		      int __scale)
{
  __builtin_ia32_scatterdiv8df (__addr, (__mmask8) 0xFF,
				(__v8di) __index, (__v8df) __v1, __scale);
}

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_mask_i64scatter_pd (void *__addr, __mmask8 __mask,
			   __m512i __index, __m512d __v1, int __scale)
{
  __builtin_ia32_scatterdiv8df (__addr, __mask, (__v8di) __index,
				(__v8df) __v1, __scale);
}

volatile __m512d src;
volatile __m512i idx;
volatile __mmask8 m8;
double *addr;

void extern
avx512f_test (void)
{
  _mm512_i64scatter_pd (addr, idx, src, 8);
  _mm512_mask_i64scatter_pd (addr, m8, idx, src, 8);
}

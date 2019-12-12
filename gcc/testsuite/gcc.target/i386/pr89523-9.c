/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mx32 -O2 -mavx512f" } */
/* { dg-final { scan-assembler-not "\tvscatter" } } */
/* { dg-final { scan-assembler "addr32 vscatter" } } */

typedef int __v8si __attribute__ ((__vector_size__ (32)));
typedef double __v8df __attribute__ ((__vector_size__ (64)));
typedef long long __m256i __attribute__ ((__vector_size__ (32),
					  __may_alias__));
typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));
typedef unsigned char  __mmask8;

extern __inline void
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm512_i32scatter_pd (void *__addr, __m256i __index, __m512d __v1,
		      int __scale)
{
  __builtin_ia32_scattersiv8df (__addr, (__mmask8) 0xFF,
				(__v8si) __index, (__v8df) __v1, __scale);
}

volatile __m512d src;
volatile __m256i idx;

void extern
avx512f_test (void)
{
  _mm512_i32scatter_pd ((void *) 0, idx, src, 8);
}

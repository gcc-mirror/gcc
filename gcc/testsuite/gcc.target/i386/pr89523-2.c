/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-mx32 -O2 -march=haswell" } */
/* { dg-final { scan-assembler "\tvgather" } } */
/* { dg-final { scan-assembler-not "addr32 vgather" } } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

extern __inline __m128d
__attribute__ ((__gnu_inline__, __always_inline__, __artificial__))
_mm_i32gather_pd (double const *__base, __m128i __index, const int __scale)
{
  __v2df __zero = { 0.0, 0.0 };
  __v2df __mask = __builtin_ia32_cmpeqpd (__zero, __zero);
  __v2df x = x;

  return (__m128d) __builtin_ia32_gathersiv2df (x,
						__base,
						(__v4si)__index,
						__mask,
						__scale);
}

__m128d x;
double *base;
__m128i idx;

void extern
avx2_test (void)
{
  x = _mm_i32gather_pd (base, idx, 1);
}

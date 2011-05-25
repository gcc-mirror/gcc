/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target sse2_runtime } */

#include <xmmintrin.h>

extern "C" void abort ();

typedef double double_a __attribute__((__may_alias__));

struct V
{
  __m128d data;
};

int
main()
{
  V a;
  __m128d b;

  b = _mm_set_pd (1., 0.);
  a.data = _mm_set_pd (1., 0.);
  a.data = _mm_add_pd (a.data,
		       _mm_and_pd (_mm_cmpeq_pd (a.data, _mm_set1_pd (0.)),
				   _mm_set1_pd (2.)));
  reinterpret_cast<double_a *>(&a.data)[1] += 1.;
  b = _mm_add_pd (b, _mm_and_pd (_mm_cmpeq_pd (b, _mm_set1_pd (0.)),
				 _mm_set1_pd (1.)));
  b = _mm_add_pd (b, _mm_and_pd (_mm_cmpeq_pd (b, _mm_set1_pd (1.)),
				 _mm_set1_pd (1.)));
  if (_mm_movemask_pd (_mm_cmpeq_pd (a.data, b)) != 0x3)
    abort();

  return 0;
}

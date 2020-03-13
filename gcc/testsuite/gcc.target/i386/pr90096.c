/* PR target/90096 */
/* { dg-do compile } */
/* { dg-options "-O0 -mno-gfni -mno-avx512f -Wno-psabi" } */

#include <x86intrin.h>

volatile __m512i x1, x2;
volatile __mmask64 m64;

void
foo (int i)
{
  x1 = _mm512_gf2p8affineinv_epi64_epi8 (x1, x2, 3);	/* { dg-error "needs isa option -mgfni -mavx512f" } */
}

#ifdef __x86_64__
unsigned long long
bar (__m128 *p)
{
  return _mm_cvtt_roundss_u64 (*p, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  /* { dg-error "needs isa option -m64 -mavx512f" "" { target lp64 } .-1 } */
  /* { dg-error "needs isa option -mx32 -mavx512f" "" { target x32 } .-2 } */
}
#endif

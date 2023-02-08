/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -mtune=generic -mtune-ctrl=dest_false_dep_for_glc -O2" } */

#include <immintrin.h>

extern __m512i i1;
extern __m256i i2;
extern __m128i i3;
extern __m512d d1, d11;
extern __m256d d2;
extern __m128d d3, d33;
extern __m512 f1, f11;
extern __m256 f2;
extern __m128 f3, f33;

__mmask16 m16;
__mmask8 m8;

#define MULLO(func, type)			\
  type						\
  mullo##type (type i2, type i1)		\
  {						\
    return func (i1, i1);			\
  }

#define MULLO_MASK(func, type)			\
  type						\
  mullo_mask##type (type i2, type i1)		\
  {						\
    return func (i1, m8, i1, i1);		\
  }

#define MULLO_MASKZ(func, type)			\
  type						\
  mullo_maksz##type (type i2, type i1)		\
  {						\
    return func (m8, i1, i1);			\
  }

MULLO (_mm512_mullo_epi64, __m512i);
MULLO_MASK (_mm512_mask_mullo_epi64, __m512i);
MULLO_MASKZ (_mm512_maskz_mullo_epi64, __m512i);
MULLO (_mm256_mullo_epi64, __m256i);
MULLO_MASK (_mm256_mask_mullo_epi64, __m256i);
MULLO_MASKZ (_mm256_maskz_mullo_epi64, __m256i);
MULLO (_mm_mullo_epi64, __m128i);
MULLO_MASK (_mm_mask_mullo_epi64, __m128i);
MULLO_MASKZ (_mm_maskz_mullo_epi64, __m128i);


__m512d
foo1 (__m512d d2, __m512d d1, __m512d d11)
{
  return _mm512_range_pd (d1, d11, 15);
}

__m512d
foo2 (__m512d d2, __m512d d1, __m512d d11)
{
  return _mm512_range_round_pd (d11, d1, 15, 8);
}

__m512d
foo3 (__m512d d2, __m512d d1, __m512d d11)
{
  return _mm512_mask_range_pd (d1, m8, d11, d11, 15);
}

__m512d
foo4 (__m512d d2, __m512d d1, __m512d d11)
{
  return _mm512_mask_range_round_pd (d11, m8, d1, d1, 15, 8);
}

__m512d
foo5 (__m512d d2, __m512d d1, __m512d d11)
{
  return _mm512_maskz_range_pd (m8, d11, d11, 15);
}

__m512d
foo6 (__m512d d2, __m512d d1, __m512d d11)
{
  return _mm512_maskz_range_round_pd (m8, d1, d1, 15, 8);
}

__m256d
foo7 (__m256d d1, __m256d d2)
{
  return _mm256_range_pd (d2, d2, 15);
}

__m256d
foo8 (__m256d d1, __m256d d2)
{
  return _mm256_mask_range_pd (d2, m8, d2, d2, 15);
}

__m256d
foo9 (__m256d d1, __m256d d2)
{
  return _mm256_maskz_range_pd (m8, d2, d2, 15);
}

__m128d
foo10 (__m128d d1, __m128d d3)
{
  return _mm_range_pd (d3, d3, 15);
}

__m128d
foo11 (__m128d d1, __m128d d3)
{
  return _mm_mask_range_pd (d3, m8, d3, d3, 15);
}

__m128d
foo12 (__m128d d1, __m128d d3)
{
  return _mm_maskz_range_pd (m8, d3, d3, 15);
}

__m128d
foo13 (__m128d d1, __m128d d33)
{
  return _mm_range_sd (d33, d33, 15);
}

__m128d
foo14 (__m128d d1, __m128d d33)
{
  return _mm_mask_range_sd (d33, m8, d33, d33, 15);
}

__m128d
foo15 (__m128d d1, __m128d d33)
{
  return _mm_maskz_range_sd (m8, d33, d33, 15);
}

__m512
bar1 (__m512 d2, __m512 d1, __m512 d11)
{
  return _mm512_range_ps (d1, d11, 15);
}

__m512
bar2 (__m512 d2, __m512 d1, __m512 d11)
{
  return _mm512_range_round_ps (d11, d1, 15, 8);
}

__m512
bar3 (__m512 d2, __m512 d1, __m512 d11)
{
  return _mm512_mask_range_ps (d1, m16, d11, d11, 15);
}

__m512
bar4 (__m512 d2, __m512 d1, __m512 d11)
{
  return _mm512_mask_range_round_ps (d11, m16, d1, d1, 15, 8);
}

__m512
bar5 (__m512 d2, __m512 d1, __m512 d11)
{
  return _mm512_maskz_range_ps (m16, d11, d11, 15);
}

__m512
bar6 (__m512 d2, __m512 d1, __m512 d11)
{
  return _mm512_maskz_range_round_ps (m16, d1, d1, 15, 8);
}

__m256
bar7 (__m256 d1, __m256 d2)
{
  return _mm256_range_ps (d2, d2, 15);
}

__m256
bar8 (__m256 d1, __m256 d2)
{
  return _mm256_mask_range_ps (d2, m8, d2, d2, 15);
}

__m256
bar9 (__m256 d1, __m256 d2)
{
  return _mm256_maskz_range_ps (m8, d2, d2, 15);
}

__m128
bar10 (__m128 d1, __m128 d3)
{
  return _mm_range_ps (d3, d3, 15);
}

__m128
bar11 (__m128 d1, __m128 d3)
{
  return _mm_mask_range_ps (d3, m8, d3, d3, 15);
}

__m128
bar12 (__m128 d1, __m128 d3)
{
  return _mm_maskz_range_ps (m8, d3, d3, 15);
}

__m128
bar13 (__m128 d1, __m128 d33)
{
  return _mm_range_ss (d33, d33, 15);
}

__m128
bar14 (__m128 d1, __m128 d33)
{
  return _mm_mask_range_ss (d33, m8, d33, d33, 15);
}

__m128
bar15 (__m128 d1, __m128 d33)
{
  return _mm_maskz_range_ss (m8, d33, d33, 15);
}

/* { dg-final { scan-assembler-times "vxorps" 26 } } */
/* { dg-final { scan-assembler-times "vpmullq" 9 } } */
/* { dg-final { scan-assembler-times "vrangepd" 12 } } */
/* { dg-final { scan-assembler-times "vrangesd" 3 } } */
/* { dg-final { scan-assembler-times "vrangeps" 12 } } */
/* { dg-final { scan-assembler-times "vrangess" 3 } } */

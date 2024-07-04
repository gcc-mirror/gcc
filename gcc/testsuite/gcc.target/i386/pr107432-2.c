/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512bw -mavx512vl -O3" } */
/* { dg-final { scan-assembler-times "vpmovsxdq" 3 } } */
/* { dg-final { scan-assembler-times "vpmovsxwq" 3 } } */
/* { dg-final { scan-assembler-times "vpmovsxbq" 3 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd" 3 } } */
/* { dg-final { scan-assembler-times "vpmovsxbd" 3 } } */
/* { dg-final { scan-assembler-times "vpmovsxbw" 3 } } */

#include <x86intrin.h>

typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef char __v4qi __attribute__ ((__vector_size__ (4)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));

__m128i mm_cvtepi32_epi64_builtin_convertvector(__v2si a)
{
  return __builtin_convertvector(a, __v2di);
}

__m256i	mm256_cvtepi32_epi64_builtin_convertvector(__v4si a)
{
  return (__m256i)__builtin_convertvector(a, __v4di);
}

__m512i	mm512_cvtepi32_epi64_builtin_convertvector(__v8si a)
{
  return (__m512i)__builtin_convertvector(a, __v8di);
}

__m128i mm_cvtepi16_epi64_builtin_convertvector(__v2hi a)
{
  return __builtin_convertvector(a, __v2di);
}

__m256i	mm256_cvtepi16_epi64_builtin_convertvector(__v4hi a)
{
  return (__m256i)__builtin_convertvector(a, __v4di);
}

__m512i	mm512_cvtepi16_epi64_builtin_convertvector(__v8hi a)
{
  return (__m512i)__builtin_convertvector(a, __v8di);
}

__m128i mm_cvtepi8_epi64_builtin_convertvector(__v2qi a)
{
  return __builtin_convertvector(a, __v2di);
}

__m256i	mm256_cvtepi8_epi64_builtin_convertvector(__v4qi a)
{
  return (__m256i)__builtin_convertvector(a, __v4di);
}

__m512i	mm512_cvtepi8_epi64_builtin_convertvector(__v8qi a)
{
  return (__m512i)__builtin_convertvector(a, __v8di);
}

__m128i mm_cvtepi16_epi32_builtin_convertvector(__v4hi a)
{
  return (__m128i)__builtin_convertvector(a, __v4si);
}

__m256i	mm256_cvtepi16_epi32_builtin_convertvector(__v8hi a)
{
  return (__m256i)__builtin_convertvector(a, __v8si);
}

__m512i	mm512_cvtepi16_epi32_builtin_convertvector(__v16hi a)
{
  return (__m512i)__builtin_convertvector(a, __v16si);
}

__m128i mm_cvtepi8_epi32_builtin_convertvector(__v4qi a)
{
  return (__m128i)__builtin_convertvector(a, __v4si);
}

__m256i	mm256_cvtepi8_epi32_builtin_convertvector(__v8qi a)
{
  return (__m256i)__builtin_convertvector(a, __v8si);
}

__m512i	mm512_cvtepi8_epi32_builtin_convertvector(__v16qi a)
{
  return (__m512i)__builtin_convertvector(a, __v16si);
}

__m128i mm_cvtepi8_epi16_builtin_convertvector(__v8qi a)
{
  return (__m128i)__builtin_convertvector(a, __v8hi);
}

__m256i	mm256_cvtepi8_epi16_builtin_convertvector(__v16qi a)
{
  return (__m256i)__builtin_convertvector(a, __v16hi);
}

__v32hi	mm512_cvtepi8_epi16_builtin_convertvector(__v32qi a)
{
  return __builtin_convertvector(a, __v32hi);
}

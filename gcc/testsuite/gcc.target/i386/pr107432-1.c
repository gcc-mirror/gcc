/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512bw -mavx512vl -O3" } */
/* { dg-final { scan-assembler-times "vpmovqd" 6 } } */
/* { dg-final { scan-assembler-times "vpmovqw" 6 } } */
/* { dg-final { scan-assembler-times "vpmovqb" 6 } } */
/* { dg-final { scan-assembler-times "vpmovdw" 6 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovdw" 8 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovdb" 6 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovdb" 8 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovwb" 8 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovwb" 10 { target { ! ia32 } } } } */

#include <x86intrin.h>

typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef char __v4qi __attribute__ ((__vector_size__ (4)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));

typedef unsigned short __v2hu __attribute__ ((__vector_size__ (4)));
typedef unsigned short __v4hu __attribute__ ((__vector_size__ (8)));
typedef unsigned char __v2qu __attribute__ ((__vector_size__ (2)));
typedef unsigned char __v4qu __attribute__ ((__vector_size__ (4)));
typedef unsigned char __v8qu __attribute__ ((__vector_size__ (8)));
typedef unsigned int __v2su __attribute__ ((__vector_size__ (8)));

__v2si mm_cvtepi64_epi32_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2si);
}

__m128i	mm256_cvtepi64_epi32_builtin_convertvector(__m256i a)
{
  return (__m128i)__builtin_convertvector((__v4di)a, __v4si);
}

__m256i	mm512_cvtepi64_epi32_builtin_convertvector(__m512i a)
{
  return (__m256i)__builtin_convertvector((__v8di)a, __v8si);
}

__v2hi	mm_cvtepi64_epi16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2hi);
}

__v4hi	mm256_cvtepi64_epi16_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v4di)a, __v4hi);
}

__m128i	mm512_cvtepi64_epi16_builtin_convertvector(__m512i a)
{
  return (__m128i)__builtin_convertvector((__v8di)a, __v8hi);
}

__v2qi	mm_cvtepi64_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2qi);
}

__v4qi	mm256_cvtepi64_epi8_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v4di)a, __v4qi);
}

__v8qi	mm512_cvtepi64_epi8_builtin_convertvector(__m512i a)
{
  return __builtin_convertvector((__v8di)a, __v8qi);
}

__v2hi	mm64_cvtepi32_epi16_builtin_convertvector(__v2si a)
{
  return __builtin_convertvector((__v2si)a, __v2hi);
}

__v4hi	mm_cvtepi32_epi16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4si)a, __v4hi);
}

__m128i	mm256_cvtepi32_epi16_builtin_convertvector(__m256i a)
{
  return (__m128i)__builtin_convertvector((__v8si)a, __v8hi);
}

__m256i	mm512_cvtepi32_epi16_builtin_convertvector(__m512i a)
{
  return (__m256i)__builtin_convertvector((__v16si)a, __v16hi);
}

__v2qi	mm64_cvtepi32_epi8_builtin_convertvector(__v2si a)
{
  return __builtin_convertvector((__v2si)a, __v2qi);
}

__v4qi	mm_cvtepi32_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4si)a, __v4qi);
}

__v8qi	mm256_cvtepi32_epi8_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v8si)a, __v8qi);
}

__m128i	mm512_cvtepi32_epi8_builtin_convertvector(__m512i a)
{
  return (__m128i)__builtin_convertvector((__v16si)a, __v16qi);
}

__v2qi	mm32_cvtepi16_epi8_builtin_convertvector(__v2hi a)
{
  return __builtin_convertvector((__v2hi)a, __v2qi);
}

__v4qi	mm64_cvtepi16_epi8_builtin_convertvector(__v4hi a)
{
  return __builtin_convertvector((__v4hi)a, __v4qi);
}

__v8qi	mm_cvtepi16_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v8hi)a, __v8qi);
}

__m128i	mm256_cvtepi16_epi8_builtin_convertvector(__m256i a)
{
  return (__m128i)__builtin_convertvector((__v16hi)a, __v16qi);
}

__m256i	mm512_cvtepi16_epi8_builtin_convertvector(__m512i a)
{
  return (__m256i)__builtin_convertvector((__v32hi)a, __v32qi);
}

__v2su mm_cvtepu64_epu32_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2du)a, __v2su);
}

__m128i	mm256_cvtepu64_epu32_builtin_convertvector(__m256i a)
{
  return (__m128i)__builtin_convertvector((__v4du)a, __v4su);
}

__m256i	mm512_cvtepu64_epu32_builtin_convertvector(__m512i a)
{
  return (__m256i)__builtin_convertvector((__v8du)a, __v8su);
}

__v2hu	mm_cvtepu64_epu16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2du)a, __v2hu);
}

__v4hu	mm256_cvtepu64_epu16_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v4du)a, __v4hu);
}

__m128i	mm512_cvtepu64_epu16_builtin_convertvector(__m512i a)
{
  return (__m128i)__builtin_convertvector((__v8du)a, __v8hu);
}

__v2qu	mm_cvtepu64_epu8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2du)a, __v2qu);
}

__v4qu	mm256_cvtepu64_epu8_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v4du)a, __v4qu);
}

__v8qu	mm512_cvtepu64_epu8_builtin_convertvector(__m512i a)
{
  return __builtin_convertvector((__v8du)a, __v8qu);
}

__v2hu	mm32_cvtepu32_epu16_builtin_convertvector(__v2su a)
{
  return __builtin_convertvector((__v2su)a, __v2hu);
}

__v4hu	mm_cvtepu32_epu16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4su)a, __v4hu);
}

__m128i	mm256_cvtepu32_epu16_builtin_convertvector(__m256i a)
{
  return (__m128i)__builtin_convertvector((__v8su)a, __v8hu);
}

__m256i	mm512_cvtepu32_epu16_builtin_convertvector(__m512i a)
{
  return (__m256i)__builtin_convertvector((__v16su)a, __v16hu);
}

__v2qu	mm32_cvtepu32_epu8_builtin_convertvector(__v2su a)
{
  return __builtin_convertvector((__v2su)a, __v2qu);
}

__v4qu	mm_cvtepu2_epu8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4su)a, __v4qu);
}

__v8qu	mm256_cvtepu32_epu8_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v8su)a, __v8qu);
}

__m128i	mm512_cvtepu32_epu8_builtin_convertvector(__m512i a)
{
  return (__m128i)__builtin_convertvector((__v16su)a, __v16qu);
}

__v2qu	mm32_cvtepu16_epu8_builtin_convertvector(__v2hu a)
{
  return __builtin_convertvector((__v2hu)a, __v2qu);
}

__v4qu	mm64_cvtepu16_epu8_builtin_convertvector(__v4hu a)
{
  return __builtin_convertvector((__v4hu)a, __v4qu);
}

__v8qu	mm_cvtepu16_epu8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v8hu)a, __v8qu);
}

__m128i	mm256_cvtepu16_epu8_builtin_convertvector(__m256i a)
{
  return (__m128i)__builtin_convertvector((__v16hu)a, __v16qu);
}

__m256i	mm512_cvtepu16_epu8_builtin_convertvector(__m512i a)
{
  return (__m256i)__builtin_convertvector((__v32hu)a, __v32qu);
}

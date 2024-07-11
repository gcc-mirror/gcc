/* { dg-do compile } */
/* { dg-options "-march=x86-64-v2 -O2" } */
/* { dg-final { scan-assembler-times "shufps" 1 } } */
/* { dg-final { scan-assembler-times "pshufb" 5 } } */

#include <x86intrin.h>

typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef char __v4qi __attribute__ ((__vector_size__ (4)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));

__v2si mm_cvtepi64_epi32_builtin_convertvector(__v2di a)
{
  return __builtin_convertvector((__v2di)a, __v2si);
}

__v2hi	mm_cvtepi64_epi16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2hi);
}

__v4hi	mm_cvtepi32_epi16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4si)a, __v4hi);
}

__v2qi	mm_cvtepi64_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2qi);
}

__v4qi	mm_cvtepi32_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4si)a, __v4qi);
}

__v8qi	mm_cvtepi16_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v8hi)a, __v8qi);
}

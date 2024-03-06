/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -O2" } */
/* { dg-final { scan-assembler-times "vshufps" 1 } } */
/* { dg-final { scan-assembler-times "vpshufb" 18 } } */
/* { dg-final { scan-assembler-times "vpermd" 1 } } */
/* { dg-final { scan-assembler-times "vpermq" 5 } } */
/* { dg-final { scan-assembler-times "vpshuflw" 1 { target { ! ia32 } } } } */

#include <x86intrin.h>

typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef char __v4qi __attribute__ ((__vector_size__ (4)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));

__v2si mm_cvtepi64_epi32_builtin_convertvector(__v2di a)
{
  return __builtin_convertvector((__v2di)a, __v2si);
}

__v4si	mm256_cvtepi64_epi32_builtin_convertvector(__v4di a)
{
  return __builtin_convertvector((__v4di)a, __v4si);
}

__v2hi	mm_cvtepi64_epi16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2hi);
}

__v4hi	mm256_cvtepi64_epi16_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v4di)a, __v4hi);
}

__v2hi	mm64_cvtepi32_epi16_builtin_convertvector(__v2si a)
{
  return __builtin_convertvector((__v2si)a, __v2hi);
}

__v4hi	mm_cvtepi32_epi16_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v4si)a, __v4hi);
}

__v8hi	mm256_cvtepi32_epi16_builtin_convertvector(__v8si a)
{
  return __builtin_convertvector((__v8si)a, __v8hi);
}

__v2qi	mm_cvtepi64_epi8_builtin_convertvector(__m128i a)
{
  return __builtin_convertvector((__v2di)a, __v2qi);
}

__v4qi	mm256_cvtepi64_epi8_builtin_convertvector(__m256i a)
{
  return __builtin_convertvector((__v4di)a, __v4qi);
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

__v16qi	mm256_cvtepi16_epi8_builtin_convertvector(__v16hi a)
{
  return __builtin_convertvector((__v16hi)a, __v16qi);
}

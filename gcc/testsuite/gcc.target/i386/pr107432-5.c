/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512dq -mavx512fp16 -mavx512vl -O3" } */
/* { dg-final { scan-assembler-times "vcvttpd2dq" 3 } } */
/* { dg-final { scan-assembler-times "vcvttps2qq" 2 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttps2qq" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttph2dq" 3 } } */
/* { dg-final { scan-assembler-times "vcvttph2qq" 3 } } */

#include <x86intrin.h>

typedef _Float16 __v2hf __attribute__ ((__vector_size__ (4)));
typedef _Float16 __v4hf __attribute__ ((__vector_size__ (8)));

__v2si mm_cvtpd_epi32_builtin_convertvector(__v2df a)
{
  return __builtin_convertvector(a, __v2si);
}

__v4si	mm256_cvtpd_epi32_builtin_convertvector(__v4df a)
{
  return __builtin_convertvector(a, __v4si);
}

__v8si	mm512_cvtpd_epi32_builtin_convertvector(__v8df a)
{
  return __builtin_convertvector(a, __v8si);
}

__v2di mm_cvtps_epi64_builtin_convertvector(__v2sf a)
{
  return __builtin_convertvector(a, __v2di);
}

__v4di	mm256_cvtps_epi64_builtin_convertvector(__v4sf a)
{
  return __builtin_convertvector(a, __v4di);
}

__v8di	mm512_cvtps_epi64_builtin_convertvector(__v8sf a)
{
  return __builtin_convertvector(a, __v8di);
}

__v4si mm_cvtph_epi32_builtin_convertvector(__v4hf a)
{
  return __builtin_convertvector(a, __v4si);
}

__v8si	mm256_cvtph_epi32_builtin_convertvector(__v8hf a)
{
  return __builtin_convertvector(a, __v8si);
}

__v16si	mm512_cvtph_epi32_builtin_convertvector(__v16hf a)
{
  return __builtin_convertvector(a, __v16si);
}

__v2di mm_cvtph_epi64_builtin_convertvector(__v2hf a)
{
  return __builtin_convertvector(a, __v2di);
}

__v4di	mm256_cvtph_epi64_builtin_convertvector(__v4hf a)
{
  return __builtin_convertvector(a, __v4di);
}

__v8di	mm512_cvtph_epi64_builtin_convertvector(__v8hf a)
{
  return __builtin_convertvector(a, __v8di);
}

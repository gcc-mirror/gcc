/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512fp16 -mavx512vl -O3" } */
/* { dg-final { scan-assembler-times "vcvtps2pd" 2 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtps2pd" 3 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtph2pd" 3 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps" 3 } } */

#include <x86intrin.h>

typedef _Float16 __v2hf __attribute__ ((__vector_size__ (4)));
typedef _Float16 __v4hf __attribute__ ((__vector_size__ (8)));

__v2df mm_cvtps_pd_builtin_convertvector(__v2sf a)
{
  return __builtin_convertvector(a, __v2df);
}

__v4df	mm256_cvtps_pd_builtin_convertvector(__v4sf a)
{
  return __builtin_convertvector(a, __v4df);
}

__v8df	mm512_cvtps_pd_builtin_convertvector(__v8sf a)
{
  return __builtin_convertvector(a, __v8df);
}

__v2df mm_cvtph_pd_builtin_convertvector(__v2hf a)
{
  return __builtin_convertvector(a, __v2df);
}

__v4df	mm256_cvtph_pd_builtin_convertvector(__v4hf a)
{
  return __builtin_convertvector(a, __v4df);
}

__v8df	mm512_cvtph_pd_builtin_convertvector(__v8hf a)
{
  return __builtin_convertvector(a, __v8df);
}

__v4sf mm_cvtph_ps_builtin_convertvector(__v4hf a)
{
  return __builtin_convertvector(a, __v4sf);
}

__v8sf	mm256_cvtph_ps_builtin_convertvector(__v8hf a)
{
  return __builtin_convertvector(a, __v8sf);
}

__v16sf	mm512_cvtph_ps_builtin_convertvector(__v16hf a)
{
  return __builtin_convertvector(a, __v16sf);
}

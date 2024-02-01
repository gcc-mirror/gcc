/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512fp16 -mavx512vl -O3" } */
/* { dg-final { scan-assembler-times "vcvtpd2ps" 3 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ph" 3 } } */
/* { dg-final { scan-assembler-times "vcvtps2ph" 3 } } */

#include <x86intrin.h>

typedef _Float16 __v2hf __attribute__ ((__vector_size__ (4)));
typedef _Float16 __v4hf __attribute__ ((__vector_size__ (8)));

__v2sf mm_cvtpd_ps_builtin_convertvector(__v2df a)
{
  return __builtin_convertvector(a, __v2sf);
}

__v4sf	mm256_cvtpd_ps_builtin_convertvector(__v4df a)
{
  return __builtin_convertvector(a, __v4sf);
}

__v8sf	mm512_cvtpd_ps_builtin_convertvector(__v8df a)
{
  return __builtin_convertvector(a, __v8sf);
}

__v2hf mm_cvtpd_ph_builtin_convertvector(__v2df a)
{
  return __builtin_convertvector(a, __v2hf);
}

__v4hf	mm256_cvtpd_ph_builtin_convertvector(__v4df a)
{
  return __builtin_convertvector(a, __v4hf);
}

__v8hf	mm512_cvtpd_ph_builtin_convertvector(__v8df a)
{
  return __builtin_convertvector(a, __v8hf);
}

__v4hf mm_cvtps_ph_builtin_convertvector(__v4sf a)
{
  return __builtin_convertvector(a, __v4hf);
}

__v8hf	mm256_cvtps_ph_builtin_convertvector(__v8sf a)
{
  return __builtin_convertvector(a, __v8hf);
}

__v16hf	mm512_cvtps_ph_builtin_convertvector(__v16sf a)
{
  return __builtin_convertvector(a, __v16hf);
}

/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -mavx512bw -O2 -mavx512dq -fno-trapping-math" } */
/* { dg-final { scan-assembler-times "vcvttpd2dq" 4 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttpd2dq" 6 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttps2dq" 6 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttps2dq" 8 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttph2w" 8 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttph2w" 10 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovdb" 10 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovdb" 14 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovwb" 8 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovwb" 10 { target { ! ia32 } } } } */

#include <x86intrin.h>

typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef char __v4qi __attribute__ ((__vector_size__ (4)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));
typedef char __v16qi __attribute__ ((__vector_size__ (16)));
typedef unsigned char __v2qu __attribute__ ((vector_size (2)));
typedef unsigned char __v4qu __attribute__ ((vector_size (4)));
typedef unsigned char __v8qu __attribute__ ((vector_size (8)));
typedef unsigned char __v16qu __attribute__ ((vector_size (16)));
typedef _Float16 __v2hf __attribute__ ((__vector_size__ (4)));
typedef _Float16 __v4hf __attribute__ ((__vector_size__ (8)));
typedef _Float16 __v8hf __attribute__ ((__vector_size__ (16)));

__v2qi	mm_cvtpd_epi8_builtin_convertvector(__v2df a)
{
  return __builtin_convertvector((__v2df)a, __v2qi);
}

__v4qi	mm256_cvtpd_epi8_builtin_convertvector(__v4df a)
{
  return __builtin_convertvector((__v4df)a, __v4qi);
}

__v8qi	mm512_cvtpd_epi8_builtin_convertvector(__v8df a)
{
  return __builtin_convertvector((__v8df)a, __v8qi);
}

__v2qu	mm_cvtpd_epu8_builtin_convertvector(__v2df a)
{
  return __builtin_convertvector((__v2df)a, __v2qu);
}

__v4qu	mm256_cvtpd_epu8_builtin_convertvector(__v4df a)
{
  return __builtin_convertvector((__v4df)a, __v4qu);
}

__v8qu	mm512_cvtpd_epu8_builtin_convertvector(__v8df a)
{
  return __builtin_convertvector((__v8df)a, __v8qu);
}

__v2qi	mm64_cvtps_epi8_builtin_convertvector(__v2sf a)
{
  return __builtin_convertvector((__v2sf)a, __v2qi);
}

__v4qi	mm128_cvtps_epi8_builtin_convertvector(__v4sf a)
{
  return __builtin_convertvector((__v4sf)a, __v4qi);
}

__v8qi	mm256_cvtps_epi8_builtin_convertvector(__v8sf a)
{
  return __builtin_convertvector((__v8sf)a, __v8qi);
}

__v16qi	mm512_cvtps_epi8_builtin_convertvector(__v16sf a)
{
  return __builtin_convertvector((__v16sf)a, __v16qi);
}

__v2qu	mm64_cvtps_epu8_builtin_convertvector(__v2sf a)
{
  return __builtin_convertvector((__v2sf)a, __v2qu);
}

__v4qu	mm128_cvtps_epu8_builtin_convertvector(__v4sf a)
{
  return __builtin_convertvector((__v4sf)a, __v4qu);
}

__v8qu	mm256_cvtps_epu8_builtin_convertvector(__v8sf a)
{
  return __builtin_convertvector((__v8sf)a, __v8qu);
}

__v16qu	mm512_cvtps_epu8_builtin_convertvector(__v16sf a)
{
  return __builtin_convertvector((__v16sf)a, __v16qu);
}

__v2qi	mm32_cvtph_epi8_builtin_convertvector(__v2hf a)
{
  return __builtin_convertvector((__v2hf)a, __v2qi);
}

__v4qi	mm64_cvtph_epi8_builtin_convertvector(__v4hf a)
{
  return __builtin_convertvector((__v4hf)a, __v4qi);
}

__v8qi	mm128_cvtph_epi8_builtin_convertvector(__v8hf a)
{
  return __builtin_convertvector((__v8hf)a, __v8qi);
}

__v16qi	mm256_cvtph_epi8_builtin_convertvector(__v16hf a)
{
  return __builtin_convertvector((__v16hf)a, __v16qi);
}

__v32qi	mm512_cvtph_epi8_builtin_convertvector(__v32hf a)
{
  return __builtin_convertvector((__v32hf)a, __v32qi);
}

__v2qu	mm32_cvtph_epu8_builtin_convertvector(__v2hf a)
{
  return __builtin_convertvector((__v2hf)a, __v2qu);
}

__v4qu	mm64_cvtph_epu8_builtin_convertvector(__v4hf a)
{
  return __builtin_convertvector((__v4hf)a, __v4qu);
}

__v8qu	mm128_cvtph_epu8_builtin_convertvector(__v8hf a)
{
  return __builtin_convertvector((__v8hf)a, __v8qu);
}

__v16qu	mm256_cvtph_epu8_builtin_convertvector(__v16hf a)
{
  return __builtin_convertvector((__v16hf)a, __v16qu);
}

__v32qu	mm512_cvtph_epu8_builtin_convertvector(__v32hf a)
{
  return __builtin_convertvector((__v32hf)a, __v32qu);
}

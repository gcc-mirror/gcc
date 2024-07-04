/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -mavx512bw -O2 -mavx512dq -fno-trapping-math" } */
/* { dg-final { scan-assembler-times "vcvtdq2pd" 4 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtdq2pd" 6 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtdq2ps" 6 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtdq2ps" 8 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtw2ph" 8 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvtw2ph" 10 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovsxbd" 5 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovsxbd" 7 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovzxbd" 5 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovzxbd" 7 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovsxbd" 5 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovsxbd" 7 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovzxbd" 5 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovzxbd" 7 { target { ! ia32 } } } } */

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

__v2df	mm_cvtepi8_pd_builtin_convertvector(__v2qi a)
{
  return __builtin_convertvector((__v2qi)a, __v2df);
}

__v4df	mm256_cvtepi8_pd_builtin_convertvector(__v4qi a)
{
  return __builtin_convertvector((__v4qi)a, __v4df);
}

__v8df	mm512_cvtepi8_pd_builtin_convertvector(__v8qi a)
{
  return __builtin_convertvector((__v8qi)a, __v8df);
}

__v2df	mm_cvtepu8_pd_builtin_convertvector(__v2qu a)
{
  return __builtin_convertvector((__v2qu)a, __v2df);
}

__v4df	mm256_cvtepu8_pd_builtin_convertvector(__v4qu a)
{
  return __builtin_convertvector((__v4qu)a, __v4df);
}

__v8df	mm512_cvtepu8_pd_builtin_convertvector(__v8qu a)
{
  return __builtin_convertvector((__v8qu)a, __v8df);
}

__v2sf	mm64_cvtepi8_ps_builtin_convertvector(__v2qi a)
{
  return __builtin_convertvector((__v2qi)a, __v2sf);
}

__v4sf	mm128_cvtepi8_ps_builtin_convertvector(__v4qi a)
{
  return __builtin_convertvector((__v4qi)a, __v4sf);
}

__v8sf	mm256_cvtepi8_ps_builtin_convertvector(__v8qi a)
{
  return __builtin_convertvector((__v8qi)a, __v8sf);
}

__v16sf	mm512_cvtepi8_ps_builtin_convertvector(__v16qi a)
{
  return __builtin_convertvector((__v16qi)a, __v16sf);
}

__v2sf	mm64_cvtepu8_ps_builtin_convertvector(__v2qu a)
{
  return __builtin_convertvector((__v2qu)a, __v2sf);
}

__v4sf	mm128_cvtepu8_ps_builtin_convertvector(__v4qu a)
{
  return __builtin_convertvector((__v4qu)a, __v4sf);
}

__v8sf	mm256_cvtepu8_ps_builtin_convertvector(__v8qu a)
{
  return __builtin_convertvector((__v8qu)a, __v8sf);
}

__v16sf	mm512_cvtepu8_ps_builtin_convertvector(__v16qu a)
{
  return __builtin_convertvector((__v16qu)a, __v16sf);
}

__v2hf	mm32_cvtepi8_ph_builtin_convertvector(__v2qi a)
{
  return __builtin_convertvector((__v2qi)a, __v2hf);
}

__v4hf	mm64_cvtepi8_ph_builtin_convertvector(__v4qi a)
{
  return __builtin_convertvector((__v4qi)a, __v4hf);
}

__v8hf	mm128_cvtepi8_ph_builtin_convertvector(__v8qi a)
{
  return __builtin_convertvector((__v8qi)a, __v8hf);
}

__v16hf	mm256_cvtepi8_ph_builtin_convertvector(__v16qi a)
{
  return __builtin_convertvector((__v16qi)a, __v16hf);
}

__v32hf	mm512_cvtepi8_ph_builtin_convertvector(__v32qi a)
{
  return __builtin_convertvector((__v32qi)a, __v32hf);
}

__v2hf	mm32_cvtepu8_ph_builtin_convertvector(__v2qu a)
{
  return __builtin_convertvector((__v2qu)a, __v2hf);
}

__v4hf	mm64_cvtepu8_ph_builtin_convertvector(__v4qu a)
{
  return __builtin_convertvector((__v4qu)a, __v4hf);
}

__v8hf	mm128_cvtepu8_ph_builtin_convertvector(__v8qu a)
{
  return __builtin_convertvector((__v8qu)a, __v8hf);
}

__v16hf	mm256_cvtepu8_ph_builtin_convertvector(__v16qu a)
{
  return __builtin_convertvector((__v16qu)a, __v16hf);
}

__v32hf	mm512_cvtepu8_ph_builtin_convertvector(__v32qu a)
{
  return __builtin_convertvector((__v32qu)a, __v32hf);
}

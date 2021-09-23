/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vcvttsh2si\[ \\t\]+\[^\{\n\]*(?:%xmm\[0-9\]|\\(%esp\\))+, %eax(?:\n|\[ \\t\]+#)" 3 } } */
/* { dg-final { scan-assembler-times "vcvttsh2usi\[ \\t\]+\[^\{\n\]*(?:%xmm\[0-9\]|\\(%esp\\))+, %eax(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvttsh2si\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+, %rax(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vcvttsh2usi\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+, %rax(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \\t\]+%edx, %edx" { target ia32 } } } */

#include <immintrin.h>

short
__attribute__ ((noinline, noclone))
trunc_f16_to_si16 (_Float16 f)
{
  return f;
}

unsigned short
__attribute__ ((noinline, noclone))
trunc_f16_to_su16 (_Float16 f)
{
  return f;
}

int
__attribute__ ((noinline, noclone))
trunc_f16_to_si32 (_Float16 f)
{
  return f;
}

unsigned int
__attribute__ ((noinline, noclone))
trunc_f16_to_su32 (_Float16 f)
{
  return f;
}

long long
__attribute__ ((noinline, noclone))
trunc_f16_to_si64 (_Float16 f)
{
  return f;
}

unsigned long long
__attribute__ ((noinline, noclone))
trunc_f16_to_su64 (_Float16 f)
{
  return f;
}

unsigned long long
__attribute__ ((noinline, noclone))
trunc_f16_to_su64_zext (_Float16 f)
{
  return (unsigned int) f;
}


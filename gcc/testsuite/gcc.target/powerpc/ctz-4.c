/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>

vector signed char
count_trailing_zeros_v16qi_1s (vector signed char a)
{
  return vec_vctz (a);
}

vector signed char
count_trailing_zeros_v16qi_2s (vector signed char a)
{
  return vec_vctzb (a);
}

vector unsigned char
count_trailing_zeros_v16qi_1u (vector unsigned char a)
{
  return vec_vctz (a);
}

vector unsigned char
count_trailing_zeros_v16qi_2u (vector unsigned char a)
{
  return vec_vctzb (a);
}

vector short
count_trailing_zeros_v8hi_1s (vector short a)
{
  return vec_vctz (a);
}

vector short
count_trailing_zeros_v8hi_2s (vector short a)
{
  return vec_vctzh (a);
}

vector unsigned short
count_trailing_zeros_v8hi_1u (vector unsigned short a)
{
  return vec_vctz (a);
}

vector unsigned short
count_trailing_zeros_v8hi_2u (vector unsigned short a)
{
  return vec_vctzh (a);
}

vector int
count_trailing_zeros_v4si_1s (vector int a)
{
  return vec_vctz (a);
}

vector int
count_trailing_zeros_v4si_2s (vector int a)
{
  return vec_vctzw (a);
}

vector unsigned int
count_trailing_zeros_v4si_1u (vector unsigned int a)
{
  return vec_vctz (a);
}

vector unsigned int
count_trailing_zeros_v4si_2u (vector unsigned int a)
{
  return vec_vctzw (a);
}

vector long long
count_trailing_zeros_v2di_1s (vector long long a)
{
  return vec_vctz (a);
}

vector long long
count_trailing_zeros_v2di_2s (vector long long a)
{
  return vec_vctzd (a);
}

vector unsigned long long
count_trailing_zeros_v2di_1u (vector unsigned long long a)
{
  return vec_vctz (a);
}

vector unsigned long long
count_trailing_zeros_v2di_2u (vector unsigned long long a)
{
  return vec_vctzd (a);
}

/* { dg-final { scan-assembler "vctzb" } } */
/* { dg-final { scan-assembler "vctzd" } } */
/* { dg-final { scan-assembler "vctzh" } } */
/* { dg-final { scan-assembler "vctzw" } } */
/* { dg-final { scan-assembler-not "cnttzd" } } */
/* { dg-final { scan-assembler-not "cnttzw" } } */

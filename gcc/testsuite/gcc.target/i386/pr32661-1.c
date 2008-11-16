/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16)));

long long foo_0(__m128i* val)
{
  return __builtin_ia32_vec_ext_v2di(*val, 0);
}

long long foo_1(__m128i* val)
{
  return __builtin_ia32_vec_ext_v2di(*val, 1);
}

/* { dg-final { scan-assembler-times "mov" 2 } } */

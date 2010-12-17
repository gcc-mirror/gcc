/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16)));

__m128i foo1(__m128i __a)
{
 return (__m128i)__builtin_ia32_pslldqi128 (__a, 8);
}

__m128i foo2(__m128i __a)
{
 return (__m128i)__builtin_ia32_psrldqi128 (__a, 8);
}

/* { dg-final { scan-assembler "psrldq" } } */
/* { dg-final { scan-assembler "pslldq" } } */

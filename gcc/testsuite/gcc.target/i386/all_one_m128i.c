/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

typedef long long __m128i __attribute__ ((__vector_size__ (16)));
typedef int __v4si __attribute__ ((__vector_size__ (16)));

__m128i foo ()
{
  __m128i minus_1 = (__m128i) (__v4si) { -1, -1, -1, -1 };

  return minus_1;
}

/* { dg-final { scan-assembler "pcmpeqd" } } */

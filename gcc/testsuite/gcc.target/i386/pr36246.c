/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -fomit-frame-pointer -mtune=generic" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef int __v4si __attribute__ ((__vector_size__ (16)));

__m128i
_mm_set_epi32 (int __q3, int __q2, int __q1, int __q0)
{
  return (__m128i)(__v4si){ __q0, __q1, __q2, __q3 };
}

/* { dg-final { scan-assembler-not "movq" } } */

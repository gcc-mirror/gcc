/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <immintrin.h>

struct S
{
  __m128 a, b;
};

struct T
{
  int a;
  struct S s;
};


void foo (struct T *p, __m128 v)
{
  struct S s;

  s = p->s;
  s.b = _mm_add_ps(s.b, v);
  p->s = s;
}

/* { dg-final { scan-assembler-not "vmovups" } } */

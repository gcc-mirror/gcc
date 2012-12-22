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
  struct S s[8];
};


void foo (struct T *p, int i, __m128 v)
{
  struct S s;

  s = p->s[i];
  s.b = _mm_add_ps(s.b, v);
  p->s[i] = s;
}

/* { dg-final { scan-assembler-not "vmovups" } } */

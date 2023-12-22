/* { dg-do compile } */
/* { dg-options "-mlsx -O2" } */

#include <lsxintrin.h>

extern void bar (__m128i, __m128i);

__m128i a;

void
foo ()
{
  bar (a, a);
}

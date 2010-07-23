/* PR middle-end/39315  */
/* { dg-do run } */
/* { dg-options "-O -msse2 -mtune=generic" } */
/* { dg-require-effective-target sse2_runtime } */
/* { dg-additional-sources pr39315-check.c } */

typedef float __m128 __attribute__ ((__vector_size__ (16)));

extern void bar (__m128 *, int);

void
foo (__m128 *x)
{
  __m128 b __attribute__ ((aligned(128))) = *x;
  bar (&b, __alignof__ (x));
}

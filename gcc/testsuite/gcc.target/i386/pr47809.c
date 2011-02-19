/* PR c/47809 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>
double bar (double, double);

__m128d
foo (__m128d x)
{
  x *= (__m128d) { bar (1.0, 1.0), 0.0 };
  return (__m128d) ((__m128i) x ^ (__m128i) { 0, 0});
}

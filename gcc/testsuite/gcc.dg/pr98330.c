/* { dg-do compile } */
/* { dg-options "-O2" } */

float f (__typeof (__builtin_pow) fn, float x)
{
  return fn (x, 2);
}

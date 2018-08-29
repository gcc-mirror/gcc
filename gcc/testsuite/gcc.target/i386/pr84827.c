/* PR target/84827 */
/* { dg-do compile } */
/* { dg-options "-Ofast -fno-fp-int-builtin-inexact -ftrapping-math -fno-associative-math -mfpmath=387" } */

double
f1 (double a)
{
  return __builtin_round (a);
}

float
f2 (float a)
{
  return __builtin_roundf (a);
}

long double
f3 (long double a)
{
  return __builtin_roundl (a);
}

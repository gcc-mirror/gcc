/* { dg-do compile } */
/* { dg-options "-ffast-math -O2" } */

double foo (double x)
{
  return __builtin_isgreater (x, 0.0) ? 0.0 : x;
}

/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

double foo(void)
{
  return __builtin_pow (0.0, -1.5);
}


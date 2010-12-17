/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

void foo (long double *x)
{
  __builtin_sincosl (*x, x, x);
}

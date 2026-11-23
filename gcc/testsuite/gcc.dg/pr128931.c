/* { dg-do compile } */
/* { dg-require-effective-target fixed_point } */
/* { dg-options "-O2" } */

void
foo (long _Fract *a, long _Fract *b)
{
  *b = *a / *a;
}

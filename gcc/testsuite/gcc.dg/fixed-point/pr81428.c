/* PR tree-optimization/81428 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (long _Fract *a, long _Fract *b)
{
  *b = *a / *a;
}

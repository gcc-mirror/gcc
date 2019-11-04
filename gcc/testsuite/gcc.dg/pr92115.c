/* PR tree-optimization/92115 */
/* { dg-do compile } */
/* { dg-options "-O1 -fexceptions -ffinite-math-only -fnon-call-exceptions -fsignaling-nans -fno-signed-zeros" } */

void
foo (double x)
{
  if (x == 0.0 && !__builtin_signbit (x))
    __builtin_abort ();
}

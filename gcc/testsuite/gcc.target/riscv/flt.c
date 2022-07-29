/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fno-trapping-math -fno-signaling-nans" } */

long
flt (double x, double y)
{
  return __builtin_isless (x, y);
}

/* { dg-final { scan-assembler "\tf(?:ge|lt)\\.d\t\[^\n\]*\n" } } */
/* { dg-final { scan-assembler-not "f\[rs\]flags" } } */

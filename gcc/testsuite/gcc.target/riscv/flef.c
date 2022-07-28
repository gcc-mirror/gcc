/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fno-trapping-math -fno-signaling-nans" } */

long
flef (float x, float y)
{
  return __builtin_islessequal (x, y);
}

/* { dg-final { scan-assembler "\tf(?:gt|le)\\.s\t\[^\n\]*\n" } } */
/* { dg-final { scan-assembler-not "f\[rs\]flags" } } */

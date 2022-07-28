/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fno-trapping-math -fno-signaling-nans" } */

long
fltf (float x, float y)
{
  return __builtin_isless (x, y);
}

/* { dg-final { scan-assembler "\tf(?:ge|lt)\\.s\t\[^\n\]*\n" } } */
/* { dg-final { scan-assembler-not "f\[rs\]flags" } } */

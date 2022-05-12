/* { dg-do compile } */
/* { dg-options "-ffinite-math-only -fsigned-zeros -dp" } */

float
sminf (float x, float y)
{
  return x <= y ? x : y;
}

/* { dg-final { scan-assembler-not "\t(call|tail)\tfminf\t" } } */
/* { dg-final { scan-assembler-not "\tfmin\\.s\t" } } */
/* { dg-final { scan-assembler "\tfle\\.s\t" } } */

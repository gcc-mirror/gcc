/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-ffinite-math-only -fno-signed-zeros -dp" } */

float
sminf (float x, float y)
{
  return x <= y ? x : y;
}

/* { dg-final { scan-assembler-not "\ttail\tfminf\t" } } */
/* { dg-final { scan-assembler-not "\tfle\\.s\t" } } */
/* { dg-final { scan-assembler "\tfmin\\.s\t\[^\n\]* sminsf3\n" } } */

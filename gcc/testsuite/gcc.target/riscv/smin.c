/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-ffinite-math-only -fno-signed-zeros -dp" } */

double
smin (double x, double y)
{
  return x <= y ? x : y;
}

/* { dg-final { scan-assembler-not "\ttail\tfmin\t" } } */
/* { dg-final { scan-assembler-not "\tfle\\.d\t" } } */
/* { dg-final { scan-assembler "\tfmin\\.d\t\[^\n\]* smindf3\n" } } */

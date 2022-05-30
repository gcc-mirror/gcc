/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-ffinite-math-only -fno-signed-zeros -dp" } */

double
smax (double x, double y)
{
  return x >= y ? x : y;
}

/* { dg-final { scan-assembler-not "\ttail\tfmax\t" } } */
/* { dg-final { scan-assembler-not "\tfge\\.d\t" } } */
/* { dg-final { scan-assembler "\tfmax\\.d\t\[^\n\]* smaxdf3\n" } } */

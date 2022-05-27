/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fsigned-zeros -fno-signaling-nans -dp" } */

double
fmin (double x, double y)
{
  return __builtin_fmin (x, y);
}

/* { dg-final { scan-assembler-not "\ttail\tfmin\t" } } */
/* { dg-final { scan-assembler-not "\tfle\\.d\t" } } */
/* { dg-final { scan-assembler "\tfmin\\.d\t\[^\n\]* fmindf3\n" } } */

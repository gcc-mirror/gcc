/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fsigned-zeros -fno-signaling-nans -dp" } */

double
fmax (double x, double y)
{
  return __builtin_fmax (x, y);
}

/* { dg-final { scan-assembler-not "\ttail\tfmax\t" } } */
/* { dg-final { scan-assembler-not "\tfge\\.d\t" } } */
/* { dg-final { scan-assembler "\tfmax\\.d\t\[^\n\]* fmaxdf3\n" } } */

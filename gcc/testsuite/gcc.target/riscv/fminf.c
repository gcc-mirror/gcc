/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fsigned-zeros -fno-signaling-nans -dp" } */

float
fminf (float x, float y)
{
  return __builtin_fminf (x, y);
}

/* { dg-final { scan-assembler-not "\ttail\tfminf\t" } } */
/* { dg-final { scan-assembler-not "\tfle\\.s\t" } } */
/* { dg-final { scan-assembler "\tfmin\\.s\t\[^\n\]* fminsf3\n" } } */

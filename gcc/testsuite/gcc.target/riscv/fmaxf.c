/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fsigned-zeros -fno-signaling-nans -dp" } */

float
fmaxf (float x, float y)
{
  return __builtin_fmaxf (x, y);
}

/* { dg-final { scan-assembler-not "\ttail\tfmaxf\t" } } */
/* { dg-final { scan-assembler-not "\tfge\\.s\t" } } */
/* { dg-final { scan-assembler "\tfmax\\.s\t\[^\n\]* fmaxsf3\n" } } */

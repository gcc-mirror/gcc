/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-fno-finite-math-only -fsigned-zeros -fsignaling-nans -dp" } */

float
fmaxf (float x, float y)
{
  return __builtin_fmaxf (x, y);
}

/* { dg-final { scan-assembler-not "\tfmax\\.s\t" } } */
/* { dg-final { scan-assembler-not "\tfge\\.s\t" } } */
/* { dg-final { scan-assembler "\t(call|tail)\tfmaxf\t" } } */

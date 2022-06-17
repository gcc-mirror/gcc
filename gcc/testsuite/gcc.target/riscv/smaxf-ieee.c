/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-ffinite-math-only -fsigned-zeros -dp" } */

float
smaxf (float x, float y)
{
  return x >= y ? x : y;
}

/* { dg-final { scan-assembler-not "\t(call|tail)\tfmaxf\t" } } */
/* { dg-final { scan-assembler-not "\tfmax\\.s\t" } } */
/* { dg-final { scan-assembler "\tfge\\.s\t" } } */

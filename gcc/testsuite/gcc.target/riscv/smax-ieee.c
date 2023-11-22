/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-ffinite-math-only -fsigned-zeros -dp" } */

double
smax (double x, double y)
{
  return x >= y ? x : y;
}

/* { dg-final { scan-assembler-not "\t(call|tail)\tfmax\t" } } */
/* { dg-final { scan-assembler-not "\tfmax\\.d\t" } } */
/* { dg-final { scan-assembler "\t(fge\\.d|flt\\.d)\t" } } */

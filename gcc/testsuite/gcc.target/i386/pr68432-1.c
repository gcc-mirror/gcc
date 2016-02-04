/* { dg-do compile } */
/* { dg-options "-O2 -fno-math-errno -fno-trapping-math -msse2 -mno-sse4 -mfpmath=sse" } */

float
f1 (float f)
{
  return __builtin_rintf (f);
}

double
f2 (double f)
{
  return __builtin_rint (f);
}

/* { dg-final { scan-assembler-times "\tucomiss\t" 1 } } */
/* { dg-final { scan-assembler-times "\tucomisd\t" 1 } } */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-math-errno -fno-trapping-math -msse2 -mno-sse4 -mfpmath=sse" } */

float __attribute__ ((cold))
f1 (float f)
{
  return __builtin_rintf (f);
}

double __attribute__ ((cold))
f2 (double f)
{
  return __builtin_rint (f);
}

/* { dg-final { scan-assembler-not "\tucomiss\t" } } */
/* { dg-final { scan-assembler-not "\tucomisd\t" } } */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -mfpmath=387" } */

double sgn (double __x)
{
  return __x == 0.0 ? 0.0 : (__x > 0.0 ? 1.0 : -1.0);
}

/* { dg-final { scan-assembler-times "fcom|ftst" 1 } } */

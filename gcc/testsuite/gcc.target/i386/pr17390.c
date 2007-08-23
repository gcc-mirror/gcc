/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -ffast-math" } */

double sgn (double __x)
{
  return __x == 0.0 ? 0.0 : (__x > 0.0 ? 1.0 : -1.0);
}

/* { dg-final { scan-assembler-times "fnstsw" 1 } } */

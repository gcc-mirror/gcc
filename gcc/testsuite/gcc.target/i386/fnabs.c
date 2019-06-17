/* PR target/62055 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */

float testf (float a)
{
  return -__builtin_fabsf (a);
}

double test (double a)
{
   return -__builtin_fabs (a);
}

__float128 testq (__float128 a)
{
   return -__builtin_fabsq (a);
}

/* { dg-final { scan-assembler-times "\tv?orp\[sd\]\[ \t\]" 2 } } */
/* { dg-final { scan-assembler-times "\tv?por\[ \t\]" 1 } } */

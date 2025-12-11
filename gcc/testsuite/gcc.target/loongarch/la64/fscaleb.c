/* { dg-do compile } */
/* { dg-options "-O2 -mabi=lp64d -mdouble-float -fno-math-errno" } */
/* { dg-final { scan-assembler-times "fscaleb\\.s" 3 } } */
/* { dg-final { scan-assembler-times "fscaleb\\.d" 4 } } */
/* { dg-final { scan-assembler-times "slli\\.w" 1 } } */

double
my_scalbln (double a, long b)
{
  return __builtin_scalbln (a, b);
}

double
my_scalbn (double a, int b)
{
  return __builtin_scalbn (a, b);
}

double
my_ldexp (double a, int b)
{
  return __builtin_ldexp (a, b);
}

float
my_scalblnf (float a, long b)
{
  return __builtin_scalblnf (a, b);
}

float
my_scalbnf (float a, int b)
{
  return __builtin_scalbnf (a, b);
}

float
my_ldexpf (float a, int b)
{
  return __builtin_ldexpf (a, b);
}

/* b must be sign-extended */
double
my_ldexp_long (double a, long b)
{
  return __builtin_ldexp (a, b);
}

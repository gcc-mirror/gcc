/* { dg-do compile { target { { powerpc*-*-* } && { ! powerpc*-apple-darwin* } } } } */
/* { dg-options "-O2 -ffast-math -mcpu=power6" } */
/* { dg-final { scan-assembler-times "fsqrt" 3 } } */
/* { dg-final { scan-assembler-times "fmul" 1 } } */
/* { dg-final { scan-assembler-times "bl pow" 1 } } */
/* { dg-final { scan-assembler-times "bl sqrt" 1 } } */

double
do_pow_0_75_default (double a)
{
  return __builtin_pow (a, 0.75);	/* should generate 2 fsqrts */
}

double
do_pow_0_5_default (double a)
{
  return __builtin_pow (a, 0.5);	/* should generate fsqrt */
}

#pragma GCC target "no-powerpc-gpopt,no-powerpc-gfxopt"

double
do_pow_0_75_nosqrt (double a)
{
  return __builtin_pow (a, 0.75);	/* should call pow */
}

double
do_pow_0_5_nosqrt (double a)
{
  return __builtin_pow (a, 0.5);	/* should call sqrt */
}

#pragma GCC reset_options

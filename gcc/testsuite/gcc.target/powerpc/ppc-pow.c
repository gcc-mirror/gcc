/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* Check for VSX here, even though we don't use VSX to eliminate SPE, PAIRED
   and other ppc floating point varients.  However, we need to also eliminate
   Darwin, since it doesn't like -mcpu=power6.  */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -ffast-math -mcpu=power6 -mno-vsx -mno-altivec" } */
/* { dg-final { scan-assembler-times "fsqrt" 3 } } */
/* { dg-final { scan-assembler-times "fmul" 1 } } */
/* { dg-final { scan-assembler-times "bl?\[\\. \]+pow" 1 } } */
/* { dg-final { scan-assembler-times "bl?\[\\. \]+sqrt" 1 } } */

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

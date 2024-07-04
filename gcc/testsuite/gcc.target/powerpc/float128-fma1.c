/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target float128 } */

__float128
xfma (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fmaf128 (a, b, c);
}

__float128
xfms (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fmaf128 (a, b, -c);
}

__float128
xfnma (__float128 a, __float128 b, __float128 c)
{
  return -__builtin_fmaf128 (a, b, c);
}

__float128
xfnms (__float128 a, __float128 b, __float128 c)
{
  return -__builtin_fmaf128 (a, b, -c);
}

/* { dg-final { scan-assembler "xsmaddqp"  } } */
/* { dg-final { scan-assembler "xsmsubqp"  } } */
/* { dg-final { scan-assembler "xsnmaddqp" } } */
/* { dg-final { scan-assembler "xsnmsubqp" } } */

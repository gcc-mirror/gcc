/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mpower9-vector -O2" } */

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

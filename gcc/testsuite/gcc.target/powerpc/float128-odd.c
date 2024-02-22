/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

/* Test the generation of the round to odd instructions.  */
__float128
f128_add(__float128 a, __float128 b)
{
  return __builtin_addf128_round_to_odd (a, b);
}

__float128
f128_sub (__float128 a, __float128 b)
{
  return __builtin_subf128_round_to_odd (a, b);
}

__float128
f128_mul (__float128 a, __float128 b)
{
  return __builtin_mulf128_round_to_odd (a, b);
}

__float128
f128_div (__float128 a, __float128 b)
{
  return __builtin_divf128_round_to_odd (a, b);
}

__float128
f128_sqrt (__float128 a)
{
  return __builtin_sqrtf128_round_to_odd (a);
}

double
f128_trunc (__float128 a)
{
  return __builtin_truncf128_round_to_odd (a);
}

__float128
f128_fma (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fmaf128_round_to_odd (a, b, c);
}

__float128
f128_fms (__float128 a, __float128 b, __float128 c)
{
  return __builtin_fmaf128_round_to_odd (a, b, -c);
}

__float128
f128_nfma (__float128 a, __float128 b, __float128 c)
{
  return - __builtin_fmaf128_round_to_odd (a, b, c);
}

__float128
f128_nfms (__float128 a, __float128 b, __float128 c)
{
  return - __builtin_fmaf128_round_to_odd (a, b, -c);
}

/* { dg-final { scan-assembler {\mxsaddqpo\M}   } } */
/* { dg-final { scan-assembler {\mxssubqpo\M}   } } */
/* { dg-final { scan-assembler {\mxsmulqpo\M}   } } */
/* { dg-final { scan-assembler {\mxsdivqpo\M}   } } */
/* { dg-final { scan-assembler {\mxssqrtqpo\M}  } } */
/* { dg-final { scan-assembler {\mxscvqpdpo\M}  } } */
/* { dg-final { scan-assembler {\mxsmaddqpo\M}  } } */
/* { dg-final { scan-assembler {\mxsmsubqpo\M}  } } */
/* { dg-final { scan-assembler {\mxsnmaddqpo\M} } } */
/* { dg-final { scan-assembler {\mxsnmsubqpo\M} } } */

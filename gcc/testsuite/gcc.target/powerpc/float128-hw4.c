/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2 -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target float128 } */

/* Insure that the ISA 3.0 IEEE 128-bit floating point built-in functions can
   be used with long double when the default is IEEE 128-bit.  */

#ifndef TYPE
#define TYPE long double
#endif

unsigned int
get_double_exponent (double a)
{
  return __builtin_vec_scalar_extract_exp (a);
}

unsigned int
get_float128_exponent (TYPE a)
{
  return __builtin_vec_scalar_extract_exp (a);
}

unsigned long
get_double_mantissa (double a)
{
  return __builtin_vec_scalar_extract_sig (a);
}

__uint128_t
get_float128_mantissa (TYPE a)
{
  return __builtin_vec_scalar_extract_sig (a);
}

double
set_double_exponent_ulong (unsigned long a, unsigned long e)
{
  return __builtin_vec_scalar_insert_exp (a, e);
}

TYPE
set_float128_exponent_uint128 (__uint128_t a, unsigned long e)
{
  return __builtin_vec_scalar_insert_exp (a, e);
}

double
set_double_exponent_double (double a, unsigned long e)
{
  return __builtin_vec_scalar_insert_exp (a, e);
}

TYPE
set_float128_exponent_float128 (TYPE a, __uint128_t e)
{
  return __builtin_vec_scalar_insert_exp (a, e);
}

TYPE
sqrt_odd (TYPE a)
{
  return __builtin_sqrtf128_round_to_odd (a);
}

double
trunc_odd (TYPE a)
{
  return __builtin_truncf128_round_to_odd (a);
}

TYPE
add_odd (TYPE a, TYPE b)
{
  return __builtin_addf128_round_to_odd (a, b);
}

TYPE
sub_odd (TYPE a, TYPE b)
{
  return __builtin_subf128_round_to_odd (a, b);
}

TYPE
mul_odd (TYPE a, TYPE b)
{
  return __builtin_mulf128_round_to_odd (a, b);
}

TYPE
div_odd (TYPE a, TYPE b)
{
  return __builtin_divf128_round_to_odd (a, b);
}

TYPE
fma_odd (TYPE a, TYPE b, TYPE c)
{
  return __builtin_fmaf128_round_to_odd (a, b, c);
}

TYPE
fms_odd (TYPE a, TYPE b, TYPE c)
{
  return __builtin_fmaf128_round_to_odd (a, b, -c);
}

TYPE
nfma_odd (TYPE a, TYPE b, TYPE c)
{
  return -__builtin_fmaf128_round_to_odd (a, b, c);
}

TYPE
nfms_odd (TYPE a, TYPE b, TYPE c)
{
  return -__builtin_fmaf128_round_to_odd (a, b, -c);
}

/* { dg-final { scan-assembler 	   {\mxsiexpdp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsiexpqp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsxexpdp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsxexpqp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsxsigdp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsxsigqp\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsaddqpo\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsdivqpo\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsmaddqpo\M}  } } */
/* { dg-final { scan-assembler 	   {\mxsmsubqpo\M}  } } */
/* { dg-final { scan-assembler 	   {\mxsmulqpo\M}   } } */
/* { dg-final { scan-assembler 	   {\mxsnmaddqpo\M} } } */
/* { dg-final { scan-assembler 	   {\mxsnmsubqpo\M} } } */
/* { dg-final { scan-assembler 	   {\mxssqrtqpo\M}  } } */
/* { dg-final { scan-assembler 	   {\mxssubqpo\M}   } } */
/* { dg-final { scan-assembler-not {\mbl\M}         } } */

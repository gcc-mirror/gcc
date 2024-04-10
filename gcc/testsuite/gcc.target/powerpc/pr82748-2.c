/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2 -mabi=ibmlongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

/* Make sure the old 'q' builtin functions work correctly when the long double
   default uses the IBM double-double format.  */

_Float128
do_fabs (_Float128 a)
{
  return __builtin_fabsq (a);
}

_Float128
do_copysign (_Float128 a, _Float128 b)
{
  return __builtin_copysignq (a, b);
}

_Float128
do_inf (void)
{
  return __builtin_infq ();
}

_Float128
do_nan (void)
{
  return __builtin_nanq ("");
}

_Float128
do_nans (void)
{
  return __builtin_nansq ("");
}

_Float128
do_huge_val (void)
{
  return __builtin_huge_valq ();
}

/* { dg-final { scan-assembler     {\mxsabsqp\M}   } } */
/* { dg-final { scan-assembler     {\mxscpsgnqp\M} } } */
/* { dg-final { scan-assembler-not {\mbl\M} }      } */

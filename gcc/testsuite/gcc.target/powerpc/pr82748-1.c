/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-mvsx -O2 -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */

/* Make sure the old 'q' builtin functions work correctly when the long double
   default has been changed to be IEEE 128-bit floating point.  */

_Float128
do_fabs_f (_Float128 a)
{
  return __builtin_fabsq (a);
}

_Float128
do_copysign_f (_Float128 a, _Float128 b)
{
  return __builtin_copysignq (a, b);
}

_Float128
do_inf_f (void)
{
  return __builtin_infq ();
}

_Float128
do_nan_f (void)
{
  return __builtin_nanq ("");
}

_Float128
do_nans_f (void)
{
  return __builtin_nansq ("");
}

_Float128
do_huge_val_f (void)
{
  return __builtin_huge_valq ();
}

long double
do_fabs_ld (long double a)
{
  return __builtin_fabsq (a);
}

long double
do_copysign_ld (long double a, long double b)
{
  return __builtin_copysignq (a, b);
}

long double
do_inf_ld (void)
{
  return __builtin_infq ();
}

long double
do_nan_ld (void)
{
  return __builtin_nanq ("");
}

long double
do_nans_ld (void)
{
  return __builtin_nansq ("");
}

long double
do_huge_val_ld (void)
{
  return __builtin_huge_valq ();
}

/* { dg-final { scan-assembler     {\mxsabsqp\M}   } } */
/* { dg-final { scan-assembler     {\mxscpsgnqp\M} } } */
/* { dg-final { scan-assembler-not {\mbl\M} }      } */

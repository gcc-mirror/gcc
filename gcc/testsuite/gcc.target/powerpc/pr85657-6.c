/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mvsx -mfloat128 -O2 -mabi=ieeelongdouble -Wno-psabi" } */

/* PR 85657 -- test that __builtin_pack_longdouble and
   __builtin_unpack_longdouble get the appropriate error messages.  */

long double
pack (double a, double b)
{
  return __builtin_pack_longdouble (a, b); /* { dg-error "builtin function '__builtin_pack_longdouble'" } */
}

double
unpack0 (long double x)
{
  return __builtin_unpack_longdouble (x, 0); /* { dg-error "builtin function '__builtin_unpack_longdouble'" } */
}

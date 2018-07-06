/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2 -mabi=ibmlongdouble -Wno-psabi" } */

/* PR 82333 was an internal compiler abort where the compiler thought that a
   long double _Complex constant was the same as __float128 _Complex.  */

_Complex long double vld;
_Complex _Float128 vf128;

_Complex long double
fld (_Complex long double arg0)
{
  return 0;
}

_Complex _Float128
ff128 (_Complex _Float128 arg0)
{
  return 0;
}

void
tld (void)
{
  vld = fld (vld);
}

void
tf128 (void)
{
  vf128 = ff128 (vf128);
}

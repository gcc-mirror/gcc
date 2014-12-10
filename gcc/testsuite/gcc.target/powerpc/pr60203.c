/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O3" } */

union u_ld { long double ld; double d[2]; };

long double
pack (double a, double aa)
{
  union u_ld u;
  u.d[0] = a;
  u.d[1] = aa;
  return u.ld;
}

double
unpack_0 (long double x)
{
  union u_ld u;
  u.ld = x;
  return u.d[0];
}

double
unpack_1 (long double x)
{
  union u_ld u;
  u.ld = x;
  return u.d[1];
}

/* { dg-final { scan-assembler-not "stfd"   } } */
/* { dg-final { scan-assembler-not "lfd"    } } */
/* { dg-final { scan-assembler-not "lxsdx"  } } */
/* { dg-final { scan-assembler-not "stxsdx" } } */
/* { dg-final { scan-assembler-not "mfvsrd" } } */
/* { dg-final { scan-assembler-not "mtvsrd" } } */



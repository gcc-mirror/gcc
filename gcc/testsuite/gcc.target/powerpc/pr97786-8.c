/* { dg-do compile } */
/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-require-effective-target powerpc_vsx } */

int test1 (long double x)
{
  return __builtin_isnormal (x);
}

/* { dg-final { scan-assembler-not {\mxscmpuqp\M} } } */
/* { dg-final { scan-assembler {\mxststdcqp\M} } } */

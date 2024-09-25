/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-require-effective-target powerpc_vsx } */

int test1 (double x)
{
  return __builtin_isinf (x);
}

int test2 (float x)
{
  return __builtin_isinf (x);
}

int test3 (float x)
{
  return __builtin_isinff (x);
}

/* { dg-final { scan-assembler-not {\mfcmp} } } */
/* { dg-final { scan-assembler-times {\mxststdcsp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxststdcdp\M} 1 } } */

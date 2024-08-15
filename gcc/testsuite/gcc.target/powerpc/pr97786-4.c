/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-require-effective-target powerpc_vsx } */

int test1 (double x)
{
  return __builtin_isfinite (x);
}

int test2 (float x)
{
  return __builtin_isfinite (x);
}

/* { dg-final { scan-assembler-not {\mfcmp} } } */
/* { dg-final { scan-assembler-times {\mxststdcsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxststdcdp\M} 1 } } */

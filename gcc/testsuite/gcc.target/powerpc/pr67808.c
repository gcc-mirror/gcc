/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O1 -mvsx -mlra -mcpu=power7 -mlong-double-128" } */

/* PR 67808: LRA ICEs on simple double to long double conversion test case */

void
dfoo (long double *ldb1, double *db1)
{
  *ldb1 = *db1;
}

long double
dfoo2 (double *db1)
{
  return *db1;
}

long double
dfoo3 (double x)
{
  return x;
}

void
ffoo (long double *ldb1, float *db1)
{
  *ldb1 = *db1;
}

long double
ffoo2 (float *db1)
{
  return *db1;
}

long double
ffoo3 (float x)
{
  return x;
}

/* { dg-final { scan-assembler "xxlxor" } } */

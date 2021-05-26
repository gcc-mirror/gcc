/* { dg-do compile } */
/* { dg-csky-options "-mcpu=ck810f -O1 -mhard-float" } */

double fldrd (double *pd, int index)
{
  return pd[index];
}

/* { dg-final { scan-assembler "fldrd" } } */

void fstrd (double *pd, int index, double d)
{
  pd[index] = d;
}

/* { dg-final { scan-assembler "fstrd" } } */


/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -mpower9-minmax" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-not "xsmaxcdp"   } } */
/* { dg-final { scan-assembler-not "xsmincdp"   } } */

double
dbl_max1 (double a, double b)
{
  return a < b ? b : a;
}

double
dbl_min1 (double a, double b)
{
  return a > b ? b : a;
}

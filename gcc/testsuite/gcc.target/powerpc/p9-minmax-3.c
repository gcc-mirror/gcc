/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -mpower9-minmax" } */
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

/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

unsigned int
get_exponent (double *p)
{
  double source = *p;

  return scalar_extract_exp (source);
}

/* { dg-final { scan-assembler "xsxexpdp" } } */

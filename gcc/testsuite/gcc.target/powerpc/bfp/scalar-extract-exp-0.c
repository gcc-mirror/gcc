/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

unsigned int
get_exponent (double *p)
{
  double source = *p;

  return scalar_extract_exp (source);
}

/* { dg-final { scan-assembler "xsxexpdp" } } */

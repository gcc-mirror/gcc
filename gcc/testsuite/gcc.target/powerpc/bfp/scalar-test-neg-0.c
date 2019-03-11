/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (double *p)
{
  double source = *p;

  return scalar_test_neg (source);
}

/* { dg-final { scan-assembler "xststdcdp" } } */

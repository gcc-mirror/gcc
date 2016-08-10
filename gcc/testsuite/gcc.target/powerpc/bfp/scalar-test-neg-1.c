/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

unsigned int
test_neg (float *p)
{
  float source = *p;

  return scalar_test_neg (source);
}

/* { dg-final { scan-assembler "xststdcsp" } } */

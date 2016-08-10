/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

unsigned int
test_data_class (float *p)
{
  float source = *p;

  return scalar_test_data_class (source, 3);
}

/* { dg-final { scan-assembler "xststdcsp" } } */

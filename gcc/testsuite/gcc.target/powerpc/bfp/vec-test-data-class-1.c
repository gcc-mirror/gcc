/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9" } */

#include <altivec.h>

__vector long long int
get_data_class_flags (__vector double *p)
{
  __vector double source = *p;

  return vec_test_data_class (source, 0x37);
}

/* { dg-final { scan-assembler "xvtstdcdp" } } */

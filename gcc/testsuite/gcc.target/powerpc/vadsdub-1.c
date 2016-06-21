/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

__vector unsigned char
doAbsoluteDifferenceUnsigned (__vector unsigned char *p,
			      __vector unsigned char *q)
{
  __vector unsigned char source_1, source_2;
  __vector unsigned char uc_result;

  source_1 = *p;
  source_2 = *q;

  uc_result = __builtin_vec_vadub (source_1, source_2);
  return uc_result;
}

/* { dg-final { scan-assembler "vabsdub" } } */

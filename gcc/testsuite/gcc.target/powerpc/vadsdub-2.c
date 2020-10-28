/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

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

  uc_result = vec_absdb (source_1, source_2);
  return uc_result;
}

/* { dg-final { scan-assembler "vabsdub" } } */

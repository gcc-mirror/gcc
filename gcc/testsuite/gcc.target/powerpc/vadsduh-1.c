/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

__vector unsigned short
doAbsoluteDifferenceUnsigned (__vector unsigned short *p,
			      __vector unsigned short *q)
{
  __vector unsigned short source_1, source_2;
  __vector unsigned short us_result;

  source_1 = *p;
  source_2 = *q;

  us_result = __builtin_vec_vaduh (source_1, source_2);
  return us_result;
}

/* { dg-final { scan-assembler "vabsduh" } } */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

__vector unsigned int
doAbsoluteDifferenceUnsignedInt (__vector unsigned int *p,
				 __vector unsigned int *q)
{
  __vector unsigned int source_1, source_2;
  __vector unsigned int result;

  source_1 = *p;
  source_2 = *q;

  result = __builtin_vec_vadu (source_1, source_2);
  return result;
}

/* { dg-final { scan-assembler "vabsduw" } } */

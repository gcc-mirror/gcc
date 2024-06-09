/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

__vector unsigned short
doAbsoluteDifferenceUnsignedMacro (__vector unsigned short *p,
				   __vector unsigned short *q)
{
  __vector unsigned short result, source_1, source_2;

  source_1 = *p;
  source_2 = *q;

  result = vec_absdh (source_1, source_2);
  return result;
}

/* { dg-final { scan-assembler "vabsduh" } } */

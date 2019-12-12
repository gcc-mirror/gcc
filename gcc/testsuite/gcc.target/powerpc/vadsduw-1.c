/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

__vector unsigned int
doAbsoluteDifferenceUnsigned (__vector unsigned int *p,
			      __vector unsigned int *q)
{
  __vector unsigned int source_1, source_2;
  __vector unsigned int ui_result;

  source_1 = *p;
  source_2 = *q;

  ui_result = __builtin_vec_vaduw (source_1, source_2);
  return ui_result;
}

/* { dg-final { scan-assembler "vabsduw" } } */

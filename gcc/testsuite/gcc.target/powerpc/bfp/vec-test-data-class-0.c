/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector bool int
get_data_class_flags (__vector float *p)
{
  __vector float source = *p;

  return vec_test_data_class (source, 0x37);
}

/* { dg-final { scan-assembler "xvtstdcsp" } } */

/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

__vector bool int
get_data_class_flags (__vector float *p, int condition_flag)
{
  __vector float source = *p;

  return vec_test_data_class (source, condition_flag); /* { dg-error "argument 2 must be a literal between 0 and 127, inclusive" } */
}

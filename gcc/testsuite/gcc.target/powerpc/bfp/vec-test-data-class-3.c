/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector bool int
get_data_class_flags (__vector float *p)
{
  __vector float source = *p;

  return __builtin_vec_test_data_class (source, 0x37); /* { dg-error "'__builtin_vsx_test_data_class_sp' requires" } */
}

/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>

__vector bool long long int
get_data_class_flags (__vector double *p)
{
  __vector double source = *p;

  return __builtin_vec_test_data_class (source, 0x37); /* { dg-error "'__builtin_vsx_test_data_class_dp' requires" } */
}

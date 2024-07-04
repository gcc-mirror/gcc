/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>
#include <stdbool.h>

bool
test_data_class (__ieee128 *p)
{
  __ieee128 source = *p;

  return __builtin_vec_scalar_test_data_class (source, 3); /* { dg-error "'__builtin_vsx_scalar_test_data_class_qp' requires" } */
}

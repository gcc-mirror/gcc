/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_data_class (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_test_data_class (source, 3); /* { dg-error "'__builtin_vsx_scalar_test_data_class_dp' requires" } */
}

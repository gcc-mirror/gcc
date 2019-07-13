/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_data_class (float *p)
{
  float source = *p;

  return __builtin_vec_scalar_test_data_class (source, 3); /* { dg-error "'__builtin_vsx_scalar_test_data_class_sp' requires" } */
}

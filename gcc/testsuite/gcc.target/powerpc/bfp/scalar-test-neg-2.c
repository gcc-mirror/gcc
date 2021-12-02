/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (float *p)
{
  float source = *p;

  return __builtin_vec_scalar_test_neg (source); /* { dg-error "'__builtin_vsx_scalar_test_neg_sp' requires" } */
}

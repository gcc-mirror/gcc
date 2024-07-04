/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_test_neg (source); /* { dg-error "'__builtin_vsx_scalar_test_neg_dp' requires" } */
}

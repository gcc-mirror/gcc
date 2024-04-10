/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (__ieee128 *p)
{
  __ieee128 source = *p;

  return __builtin_vec_scalar_test_neg (source); /* { dg-error "'__builtin_vsx_scalar_test_neg_qp' requires" } */
}

/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_neg (float *p)
{
  float source = *p;

  return __builtin_vec_scalar_test_neg_sp (source); /* { dg-error "Builtin function __builtin_vsx_scalar_test_neg_sp requires" } */
}

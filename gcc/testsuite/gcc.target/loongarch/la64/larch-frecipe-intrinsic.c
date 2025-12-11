/* Test intrinsics for frecipe.{s/d} and frsqrte.{s/d} instructions */
/* { dg-do compile } */
/* { dg-options "-mfrecipe -O2" } */
/* { dg-final { scan-assembler-times "test_frecipe_s:.*frecipe\\.s.*test_frecipe_s" 1 } } */
/* { dg-final { scan-assembler-times "test_frecipe_d:.*frecipe\\.d.*test_frecipe_d" 1 } } */
/* { dg-final { scan-assembler-times "test_frsqrte_s:.*frsqrte\\.s.*test_frsqrte_s" 1 } } */
/* { dg-final { scan-assembler-times "test_frsqrte_d:.*frsqrte\\.d.*test_frsqrte_d" 1 } } */

#include <larchintrin.h>

float
test_frecipe_s (float _1)
{
  return __frecipe_s (_1);
}
double
test_frecipe_d (double _1)
{
  return __frecipe_d (_1);
}
float
test_frsqrte_s (float _1)
{
  return __frsqrte_s (_1);
}
double
test_frsqrte_d (double _1)
{
  return __frsqrte_d (_1);
}

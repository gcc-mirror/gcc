/* Test builtins for xvfrecipe.{s/d} and xvfrsqrte.{s/d} instructions */
/* { dg-do compile } */
/* { dg-options "-mlasx -mfrecipe" } */
/* { dg-final { scan-assembler-times "lasx_xvfrecipe_s:.*xvfrecipe\\.s.*lasx_xvfrecipe_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrecipe_d:.*xvfrecipe\\.d.*lasx_xvfrecipe_d" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrsqrte_s:.*xvfrsqrte\\.s.*lasx_xvfrsqrte_s" 1 } } */
/* { dg-final { scan-assembler-times "lasx_xvfrsqrte_d:.*xvfrsqrte\\.d.*lasx_xvfrsqrte_d" 1 } } */

#include <lasxintrin.h>

v8f32
__lasx_xvfrecipe_s (v8f32 _1)
{
  return __builtin_lasx_xvfrecipe_s (_1);
}
v4f64
__lasx_xvfrecipe_d (v4f64 _1)
{
  return __builtin_lasx_xvfrecipe_d (_1);
}
v8f32
__lasx_xvfrsqrte_s (v8f32 _1)
{
  return __builtin_lasx_xvfrsqrte_s (_1);
}
v4f64
__lasx_xvfrsqrte_d (v4f64 _1)
{
  return __builtin_lasx_xvfrsqrte_d (_1);
}

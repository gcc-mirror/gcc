/* Test builtins for vfrecipe.{s/d} and vfrsqrte.{s/d} instructions */
/* { dg-do compile } */
/* { dg-options "-mlsx -mfrecipe" } */
/* { dg-final { scan-assembler-times "lsx_vfrecipe_s:.*vfrecipe\\.s.*lsx_vfrecipe_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrecipe_d:.*vfrecipe\\.d.*lsx_vfrecipe_d" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrsqrte_s:.*vfrsqrte\\.s.*lsx_vfrsqrte_s" 1 } } */
/* { dg-final { scan-assembler-times "lsx_vfrsqrte_d:.*vfrsqrte\\.d.*lsx_vfrsqrte_d" 1 } } */

#include <lsxintrin.h>

v4f32
__lsx_vfrecipe_s (v4f32 _1)
{
  return __builtin_lsx_vfrecipe_s (_1);
}
v2f64
__lsx_vfrecipe_d (v2f64 _1)
{
  return __builtin_lsx_vfrecipe_d (_1);
}
v4f32
__lsx_vfrsqrte_s (v4f32 _1)
{
  return __builtin_lsx_vfrsqrte_s (_1);
}
v2f64
__lsx_vfrsqrte_d (v2f64 _1)
{
  return __builtin_lsx_vfrsqrte_d (_1);
}

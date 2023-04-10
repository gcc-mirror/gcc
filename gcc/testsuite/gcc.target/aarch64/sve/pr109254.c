/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -funroll-loops" } */

#include <arm_sve.h>

svfloat32_t __attribute__((noipa))
func_demo (svfloat32_t x, svfloat32_t y, svbool_t pg)
{
  svfloat32_t z = svadd_f32_x (pg, x, svdup_f32 (0x1.800fep19f));
  svbool_t cmp = svcmplt_f32 (pg, z, svdup_f32 (0.0f));
  svfloat32_t zM1 = svsub_f32_x (pg, z, svdup_n_f32 (1.0f));
  z = svsel_f32 (cmp, zM1, z);
  svfloat32_t sum = svadd_f32_x (pg, z, y);
  return sum;
}

int
main ()
{
  float arr[2];
  svfloat32_t x = svinsr_n_f32 (svdup_f32 (-0x1.880fep19f), 2.0f);
  svfloat32_t res = func_demo (x, svdup_f32 (0.5f), svptrue_b32 ());
  svst1_f32 (svptrue_pat_b32 (SV_VL2), arr, res);
  if (arr[0] != 786561.5f || arr[1] != -16384.5f)
    __builtin_abort ();
  return 0;
}

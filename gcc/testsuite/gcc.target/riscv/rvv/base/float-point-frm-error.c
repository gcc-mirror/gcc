/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef float float32_t;

void test_float_point_frm_error (float32_t *out, vfloat32m1_t op1, vfloat32m1_t op2, size_t vl)
{
  vfloat32m1_t v1 = __riscv_vfadd_vv_f32m1_rm (op1, op2, 5, vl); /* { dg-error {passing 5 to argument 3 of '__riscv_vfadd_vv_f32m1_rm', which expects a value in the range \[0, 4\]} } */
  vfloat32m1_t v2 = __riscv_vfadd_vv_f32m1_rm (v1, v1, 6, vl);   /* { dg-error {passing 6 to argument 3 of '__riscv_vfadd_vv_f32m1_rm', which expects a value in the range \[0, 4\]} } */
  vfloat32m1_t v3 = __riscv_vfadd_vv_f32m1_rm (v2, v2, 8, vl);   /* { dg-error {passing 8 to argument 3 of '__riscv_vfadd_vv_f32m1_rm', which expects a value in the range \[0, 4\]} } */

  __riscv_vse32_v_f32m1 (out, v3, vl);
}

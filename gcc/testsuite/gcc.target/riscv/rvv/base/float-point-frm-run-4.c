/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3" } */

#include "riscv_vector.h"
#include <stdio.h>
#include <stdint-gcc.h>

#include "float-point-frm-run.h"

#define ORIGINAL_FRM 1

vfloat32m1_t __attribute__ ((noinline))
other_function (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl)
{
  vfloat32m1_t result = op2;

  result = __riscv_vfadd_vv_f32m1 (op1, result, vl);

  assert_equal (ORIGINAL_FRM, get_frm (), "The value of frm should be equal");

  return result;
}

vfloat32m1_t __attribute__ ((noinline))
test_float_point_frm_run (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl)
{
  vfloat32m1_t result = {};

  result = __riscv_vfadd_vv_f32m1_rm (op1, result, 4, vl);

  assert_equal (4, get_frm (), "The value of frm should be equal");

  return other_function (result, op2, vl);
}

int
main ()
{
  size_t vl = 8;
  vfloat32m1_t op1 = {};
  vfloat32m1_t op2 = {};

  set_frm (ORIGINAL_FRM);
  test_float_point_frm_run (op1, op2, vl);

  return 0;
}

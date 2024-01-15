/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3" } */

#include "riscv_vector.h"
#include <stdio.h>
#include <stdint-gcc.h>

#include "float-point-frm-run.h"

#define ORIGINAL_FRM 1
#define NEW_FRM 4

void __attribute__ ((noinline))
other_function ()
{
  set_frm (NEW_FRM);
}

vfloat32m1_t __attribute__ ((noinline))
test_float_point_frm_run (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl)
{
  vfloat32m1_t result = {};

  other_function ();
  assert_equal (NEW_FRM, get_frm (), "The value of frm should be equal");

  result = __riscv_vfadd_vv_f32m1_rm (op1, result, 2, vl);
  assert_equal (2, get_frm (), "The value of frm should be equal");

  result = __riscv_vfadd_vv_f32m1 (op1, result, vl);
  assert_equal (NEW_FRM, get_frm (), "The value of frm should be equal");

  return result;
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

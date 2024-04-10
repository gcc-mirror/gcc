/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3" } */

#include "riscv_vector.h"
#include <stdio.h>
#include <stdint-gcc.h>

#include "float-point-frm-run.h"

#define DEFINE_TEST_FUNC(FRM) \
vfloat32m1_t __attribute__ ((noinline)) \
test_float_point_frm_run_##FRM (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl) \
{                                                                              \
  vfloat32m1_t result;                                                         \
                                                                               \
  set_frm (0);                                                                 \
                                                                               \
  result = __riscv_vfadd_vv_f32m1_rm (op1, result, FRM, vl);                   \
                                                                               \
  assert_equal (FRM, get_frm (), "The value of frm should be equal");          \
                                                                               \
  return result;                                                               \
}

#define RUN_TEST_FUNC(FRM, op1, op2, vl) \
  test_float_point_frm_run_##FRM (op1, op2, vl)

DEFINE_TEST_FUNC (0)
DEFINE_TEST_FUNC (1)
DEFINE_TEST_FUNC (2)
DEFINE_TEST_FUNC (3)
DEFINE_TEST_FUNC (4)

int
main ()
{
  size_t vl = 8;
  vfloat32m1_t op1;
  vfloat32m1_t op2;

  set_frm (4);

  RUN_TEST_FUNC (0, op1, op2, vl);
  assert_equal (4, get_frm (), "The value of frm should be equal");

  RUN_TEST_FUNC (1, op1, op2, vl);
  assert_equal (4, get_frm (), "The value of frm should be equal");

  RUN_TEST_FUNC (2, op1, op2, vl);
  assert_equal (4, get_frm (), "The value of frm should be equal");

  RUN_TEST_FUNC (3, op1, op2, vl);
  assert_equal (4, get_frm (), "The value of frm should be equal");

  RUN_TEST_FUNC (4, op1, op2, vl);
  assert_equal (4, get_frm (), "The value of frm should be equal");

  return 0;
}

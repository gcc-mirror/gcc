/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vint8m1_t
__attribute__((target("arch=+zve64d")))
test_1 (vint8m1_t a, vint8m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i8m1 (a, b, vl);
}

vint16m1_t
__attribute__((target("arch=+zve64d")))
test_2 (vint16m1_t a, vint16m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i16m1 (a, b, vl);
}

vint32m1_t
__attribute__((target("arch=+zve64d")))
test_3 (vint32m1_t a, vint32m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i32m1 (a, b, vl);
}

vint64m1_t
__attribute__((target("arch=+zve64d")))
test_4 (vint64m1_t a, vint64m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i64m1 (a, b, vl);
}

vfloat32m1_t
__attribute__((target("arch=+zve64d")))
test_5 (vfloat32m1_t a, vfloat32m1_t b, size_t vl)
{
  return __riscv_vfadd_vv_f32m1 (a, b, vl);
}

vfloat64m1_t
__attribute__((target("arch=+zve64d")))
test_6 (vfloat64m1_t a, vfloat64m1_t b, size_t vl)
{
  return __riscv_vfadd_vv_f64m1 (a, b, vl);
}

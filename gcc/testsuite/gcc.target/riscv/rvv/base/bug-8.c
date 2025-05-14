/* Test that we do not have ice when compile */
/* { dg-do assemble } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O0"  { target { rv64 } } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O0"  { target { rv32 } } } */

#include "riscv_vector.h"

vint64m1_t f1 (vint64m1_t vd, vint64m1_t vs2, size_t vl)
{
  return __riscv_vmacc_vx_i64m1 (vd, 0, vs2, vl);
}

vint64m1_t f2 (vint64m1_t vd, vint64m1_t vs2, size_t vl)
{
  return __riscv_vnmsac_vx_i64m1 (vd, 0, vs2, vl);
}

vint64m8_t f3 (vint64m8_t vd, vint64m8_t vs2, size_t vl)
{
  return __riscv_vmadd_vx_i64m8 (vd, 0, vs2, vl);
}

vint64m1_t f4 (vint64m1_t vd, vint64m1_t vs2, size_t vl)
{
  return __riscv_vnmsub_vx_i64m1 (vd, 0, vs2, vl);
}

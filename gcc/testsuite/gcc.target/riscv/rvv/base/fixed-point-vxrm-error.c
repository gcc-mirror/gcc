/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vint8mf8_t test_vsadd_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vsadd_vv_i8mf8 (op1, op2, __RISCV_VXRM_RNU, vl); /* { dg-error {too many arguments to function '__riscv_vsadd_vv_i8mf8'} } */
}

vuint8mf8_t test_vsaddu_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t op2, size_t vl)
{
  return __riscv_vsaddu_vv_u8mf8 (op1, op2, __RISCV_VXRM_RNU, vl); /* { dg-error {too many arguments to function '__riscv_vsaddu_vv_u8mf8'} } */
}

vint8mf8_t test_vssub_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vssub_vv_i8mf8 (op1, op2, __RISCV_VXRM_RNU, vl); /* { dg-error {too many arguments to function '__riscv_vssub_vv_i8mf8'} } */
}

vuint8mf8_t test_vssubu_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t op2, size_t vl)
{
  return __riscv_vssubu_vv_u8mf8 (op1, op2, __RISCV_VXRM_RNU, vl); /* { dg-error {too many arguments to function '__riscv_vssubu_vv_u8mf8'} } */
}

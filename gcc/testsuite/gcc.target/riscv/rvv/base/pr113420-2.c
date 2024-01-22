/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vint8mf8_t
test_vle8_v_i8mf8_m (vbool64_t vm, const int8_t *rs1, size_t vl)
{
  return __riscv_vle8 (vm, rs1, vl);
}

vuint8mf8_t
test_vle8_v_u8mf8_m (vbool64_t vm, const uint8_t *rs1, size_t vl)
{
  return __riscv_vle8 (vm, rs1, vl);
}

vfloat32mf2_t
test_vfadd_vv_f32mf2 (vfloat32mf2_t vs2, vfloat32mf2_t vs1, size_t vl)
{
  return __riscv_vfadd (vs2, vs1, vl);
}

vfloat32mf2_t
test_vfadd_vv_f32mf2_rm (vfloat32mf2_t vs2, vfloat32mf2_t vs1, size_t vl)
{
  return __riscv_vfadd (vs2, vs1, __RISCV_FRM_RNE, vl);
}

/* { dg-final { scan-assembler-times {vle8\.v} 2 } } */
/* { dg-final { scan-assembler-times {vfadd\.v} 2 } } */

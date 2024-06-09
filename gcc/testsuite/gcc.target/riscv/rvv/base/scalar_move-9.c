/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -fno-schedule-insns -fno-schedule-insns2 -O3" } */

#include "riscv_vector.h"

vuint64m2_t f1(vuint64m2_t var_17, uint64_t var_60)
{
  vuint64m2_t var_16 = __riscv_vmv_s_x_u64m2_tu(var_17,var_60, 0);
  return var_16;
}

vuint64m2_t f2(vuint64m2_t var_17, uint64_t var_60)
{
  vuint64m2_t var_16 = __riscv_vmv_s_x_u64m2_tu(var_17,var_60, 4);
  return var_16;
}

vuint64m2_t f3(vuint64m2_t var_17, uint64_t var_60, size_t vl)
{
  vuint64m2_t var_16 = __riscv_vmv_s_x_u64m2_tu(var_17,var_60, vl);
  return var_16;
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*0,\s*e64,\s*m2,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*1,\s*e64,\s*m2,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {sgtu} 1 } } */

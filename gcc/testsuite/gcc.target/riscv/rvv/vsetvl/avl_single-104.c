/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void
foo (int cond, int vl, int *in, int *out, int n)
{
  if (cond > 30)
    {
      vint32m1_t v = __riscv_vle32_v_i32m1 ((int32_t *) in, vl);
      __riscv_vse32_v_i32m1 ((int32_t *) out, v, vl);
    }
  else if (cond < 10)
    {
      vint8mf4_t v = __riscv_vle8_v_i8mf4 ((int8_t *) in, vl);
      v = __riscv_vle8_v_i8mf4_tu (v, (int8_t *) in + 10, vl);
      __riscv_vse8_v_i8mf4 ((int8_t *) out, v, vl);
    }
  else
    {
      vl = vl * 2;
    }

  for (int i = 0; i < n; i += 1)
    {
      vint16mf2_t v = __riscv_vle16_v_i16mf2 ((int16_t *) in + i, vl);
      v = __riscv_vle16_v_i16mf2_tu (v, (int16_t *) in + i + 10, vl);
      v = __riscv_vadd_vv_i16mf2 (v, v, vl);
      __riscv_vse16_v_i16mf2 ((int16_t *) out + i, v, vl);
    }
}

/* { dg-final { scan-assembler-not {vsetvli\s+zero,zero,e16,mf2,t[au],m[au]} { target { no-opts "-O0" no-opts "-Os" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */

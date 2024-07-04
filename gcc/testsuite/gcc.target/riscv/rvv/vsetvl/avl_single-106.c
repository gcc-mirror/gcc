/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void
foo (int vl, int n, int m, int32_t *in, int32_t *out)
{
  size_t avl;
  if (m > 10)
    {
      avl = __riscv_vsetvl_e8mf4 (vl);
      vint8mf4_t v = __riscv_vle8_v_i8mf4 ((int8_t *) in + 10, avl);
      v = __riscv_vadd_vv_i8mf4 (v, v, avl);
      __riscv_vse8_v_i8mf4 ((int8_t *) out + 10, v, avl);
    }
  else
    {
      avl = __riscv_vsetvl_e16mf2 (vl);
      vint16mf2_t v = __riscv_vle16_v_i16mf2 ((int16_t *) in + 10, avl);
      v = __riscv_vadd_vv_i16mf2 (v, v, avl);
      __riscv_vse16_v_i16mf2 ((int16_t *) out + 10, v, avl);
    }

  for (int i = 0; i < n; i += 1)
    {
      vint32m1_t v = __riscv_vle32_v_i32m1 (in + i, avl);
      v = __riscv_vadd_vv_i32m1 (v, v, avl);
      __riscv_vse32_v_i32m1 (out + i, v, avl);
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 3 { target { { any-opts "-O2" "-O3" } && { no-opts "-g" "-funroll-loops" } } } } } */
/* { dg-final { scan-assembler-times {vsetvli\tzero,zero,e32,m1,t[au],m[au]} 1 { target { { any-opts "-O2" "-O3" } && { no-opts "-g" "-funroll-loops" } } } } } */

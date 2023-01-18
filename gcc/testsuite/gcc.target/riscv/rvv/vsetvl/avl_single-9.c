/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int l, int n, int m)
{
  int vl = 32;
  for (int i = 0; i < n; i++)
    {
      vint8mf8_t v1 = __riscv_vle8_v_i8mf8 (in + i + 1, vl);
      __riscv_vse8_v_i8mf8 (out + i + 1, v1, vl);

      vint8mf8_t v2 = __riscv_vle8_v_i8mf8 (in + i + 2, vl);
      __riscv_vse8_v_i8mf8 (out + i + 2, v2, vl);

      vint8mf8_t v3 = __riscv_vle8_v_i8mf8 (in + i + 3, vl);
      __riscv_vse8_v_i8mf8 (out + i + 3, v3, vl);

      vint8mf8_t v4 = __riscv_vle8_v_i8mf8 (in + i + 4, vl);
      __riscv_vse8_v_i8mf8 (out + i + 4, v4, vl);

      vint8mf8_t v5 = __riscv_vle8_v_i8mf8 (in + i + 5, vl);
      __riscv_vse8_v_i8mf8 (out + i + 5, v5, vl);

      vint8mf8_t v6 = __riscv_vle8_v_i8mf8 (in + i + 6, vl);
      __riscv_vse8_v_i8mf8 (out + i + 6, v6, vl);

      vint8mf8_t v7 = __riscv_vle8_v_i8mf8 (in + i + 7, vl);
      __riscv_vse8_v_i8mf8 (out + i + 7, v7, vl);

      vint8mf8_t v8 = __riscv_vle8_v_i8mf8 (in + i + 8, vl);
      __riscv_vse8_v_i8mf8 (out + i + 8, v8, vl);

      vint8mf8_t v9 = __riscv_vle8_v_i8mf8 (in + i + 9, vl);
      __riscv_vse8_v_i8mf8 (out + i + 9, v9, vl);

      vint8mf8_t v10 = __riscv_vle8_v_i8mf8 (in + i + 10, vl);
      __riscv_vse8_v_i8mf8 (out + i + 10, v10, vl);

      vint8mf8_t v11 = __riscv_vle8_v_i8mf8 (in + i + 11, vl);
      __riscv_vse8_v_i8mf8 (out + i + 11, v11, vl);

      vint8mf8_t v12 = __riscv_vle8_v_i8mf8 (in + i + 12, vl);
      __riscv_vse8_v_i8mf8 (out + i + 12, v12, vl);

      vint8mf8_t v13 = __riscv_vle8_v_i8mf8 (in + i + 13, vl);
      __riscv_vse8_v_i8mf8 (out + i + 13, v13, vl);

      vint8mf8_t v14 = __riscv_vle8_v_i8mf8 (in + i + 14, vl);
      __riscv_vse8_v_i8mf8 (out + i + 14, v14, vl);
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,32} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

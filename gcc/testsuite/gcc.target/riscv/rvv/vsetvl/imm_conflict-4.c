/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n, int cond)
{
  if (n > cond) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 600, 5);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + 600, 5);
    __riscv_vse8_v_i8mf8 (out + 600, v2, 5);
  } else {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 700, 5);
    __riscv_vse8_v_i8mf8 (out + 700, v, 5);
  }

  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = *(vint8mf8_t*) (in + 900 + i);
    *(vint8mf8_t*) (out + 900 + i) = v;
  }

  for (int i = 0 ; i < n; i++) {
    vint32m1_t v = __riscv_vle32_v_i32m1 (in + 1000 + i, 19);
    __riscv_vse32_v_i32m1 (out + 1000 + i, v, 19);
  }

  for (int i = 0 ; i < n * n; i++) {
    vint32m1_t v = __riscv_vle32_v_i32m1 (in + 2000 + i, 8);
    __riscv_vse32_v_i32m1 (out + 2000 + i, v, 8);
  }
}

/* { dg-final { scan-assembler {vsetivli\s+zero,\s*5,\s*e8,\s*mf8,\s*tu,\s*m[au]} { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler {vsetivli\s+zero,\s*19,\s*e32,\s*m1,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler {vsetivli\s+zero,\s*8,\s*e32,\s*m1,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz"  no-opts "-funroll-loops" no-opts "-g" } } } } */

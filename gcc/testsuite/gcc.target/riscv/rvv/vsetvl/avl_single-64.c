/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n, int cond)
{
  size_t vl = 777;
  if (n > cond) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 600, vl);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in + 600, vl);
    __riscv_vse8_v_i8mf8 (out + 600, v2, vl);
  } else {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + 700, vl);
    __riscv_vse8_v_i8mf8 (out + 700, v, vl);
  }

  for (int i = 0 ; i < n * n * n * n; i++) {
    vint8mf8_t v = *(vint8mf8_t*) (in + 900 + i);
    *(vint8mf8_t*) (out + 900 + i) = v;
  }

  for (int i = 0 ; i < n; i++) {
    vint32m1_t v = __riscv_vle32_v_i32m1 (in + 1000 + i, vl);
    __riscv_vse32_v_i32m1 (out + 1000 + i, v, vl);
  }
  
  vl = 888;
  for (int i = 0 ; i < n; i++) {
    vint32m1_t v = __riscv_vle32_v_i32m1 (in + 1000 + i, vl);
    __riscv_vse32_v_i32m1 (out + 1000 + i, v, vl);
  }
  
  vl = 444;
  for (int i = 0 ; i < n * n; i++) {
    vint32m1_t v = __riscv_vle32_v_i32m1 (in + 2000 + i, vl);
    __riscv_vse32_v_i32m1 (out + 2000 + i, v, vl);
  }
}

/* { dg-final { scan-assembler-times {vsetvli} 6 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-g" } } } } */

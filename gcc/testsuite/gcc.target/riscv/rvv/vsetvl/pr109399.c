/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void foo(void *in1, void *in2, void *in3, void *out, size_t n) {
  size_t vl = __riscv_vsetvlmax_e32m1();
  vint32m1_t a = __riscv_vle32_v_i32m1(in1, vl);
  vint32m1_t b = __riscv_vle32_v_i32m1_tu(a, in2, vl);
  vint32m1_t c = __riscv_vle32_v_i32m1_tu(b, in3, vl);
  __riscv_vse32_v_i32m1(out, c, vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m1,\s*tu,\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

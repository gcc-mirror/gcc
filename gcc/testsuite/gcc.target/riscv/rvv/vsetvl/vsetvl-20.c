/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f(int32_t *in1, int32_t *in2, int32_t *in3, int32_t *out, size_t n, int cond, int avl) {
  
  size_t vl;
  if (cond)
    vl = __riscv_vsetvl_e32m1(avl);
  else
    vl = (__riscv_vsetvl_e32m1(avl) << 5) + 100;
  for (size_t i = 0; i < n; i += 1) {
    vint32m1_t a = __riscv_vle32_v_i32m1(in1, vl);
    vint32m1_t b = __riscv_vle32_v_i32m1_tu(a, in2, vl);
    vint32m1_t c = __riscv_vle32_v_i32m1_tu(b, in3, vl);
    __riscv_vse32_v_i32m1(out, c, vl);
  }
}

/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {slli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*5} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {addi\s+[a-x0-9]+,\s*[a-x0-9]+,\s*100} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

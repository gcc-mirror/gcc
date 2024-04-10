/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void foo(int32_t *in1, int32_t *in2, int32_t *in3, int32_t *out, size_t n, int cond, int vl) {
  
  vint32m1_t a = __riscv_vle32_v_i32m1(in1, __riscv_vsetvl_e8mf8 (vl));
  vint32m1_t b = __riscv_vle32_v_i32m1_tu(a, in2, __riscv_vsetvl_e8mf4 (vl));
  vint32m1_t c = __riscv_vle32_v_i32m1_tu(b, in3, __riscv_vsetvl_e8m1 (vl));
  __riscv_vse32_v_i32m1(out, c, __riscv_vsetvl_e8mf2 (vl));
}

/* { dg-final { scan-assembler-times {vsetvli} 7 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

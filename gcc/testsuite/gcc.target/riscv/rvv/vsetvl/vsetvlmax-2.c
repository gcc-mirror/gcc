/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void foo(int32_t *in1, int32_t *in2, int32_t *in3, int32_t *out, size_t n) {
  for (size_t i = 0; i < n; i += 1) {
    vint32m1_t a = __riscv_vle32_v_i32m1(in1, __riscv_vsetvlmax_e32m1());
    __riscv_vse32_v_i32m1(out, a, __riscv_vsetvlmax_e32m1());
  }
  
  for (size_t i = 0; i < n; i += 1) {
    vint32m1_t a = __riscv_vle32_v_i32m1(in1 + 100, __riscv_vsetvlmax_e32m1());
    vint32m1_t b = __riscv_vle32_v_i32m1_tu(a, in2 + 100, __riscv_vsetvlmax_e32m1());
    vint32m1_t c = __riscv_vle32_v_i32m1_tu(b, in3 + 100, __riscv_vsetvlmax_e32m1());
    __riscv_vse32_v_i32m1(out + 100, c, __riscv_vsetvlmax_e32m1());
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m1,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */

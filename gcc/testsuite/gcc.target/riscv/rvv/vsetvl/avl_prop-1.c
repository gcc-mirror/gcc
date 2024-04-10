/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void
foo (void *a, void *b, void *c, size_t n)
{
  for (size_t vl; n > 0; n -= vl, a += vl, b += vl * 4, c += vl)
    {
      vl = __riscv_vsetvl_e8mf4 (n);
      vint32m1_t vec_b = __riscv_vle32_v_i32m1 (b, vl);
      vint32m1_t vec_a = __riscv_vadd_vv_i32m1 (vec_b, vec_b, __riscv_vsetvlmax_e32m1 ());
      __riscv_vse32_v_i32m1 (a, vec_a, vl);
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */

/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize -frename-registers" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int n, int m, unsigned cond, size_t vl, double scalar)
{
  size_t new_vl = 101;
  
  vfloat64m4_t v2 = __riscv_vle64_v_f64m4 ((double *)in, new_vl);
  double f = __riscv_vfmv_f_s_f64m4_f64 (v2);
  
  for (size_t i = 0; i < n; i++)
    {
      vfloat64m4_t v3 = __riscv_vle64_v_f64m4 ((double *)(in + i + 500), new_vl);
      vfloat64m4_t v4 = __riscv_vle64_v_f64m4 ((double *)(in + i + 600), new_vl);
      v4 = __riscv_vfmacc_vf_f64m4 (v4, f, v3, new_vl);
      
      __riscv_vse64_v_f64m4 ((double *)(out + i + 200), v4, new_vl);
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-not {vsetivli} { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-g" no-opts "-funroll-loops" } } } } */

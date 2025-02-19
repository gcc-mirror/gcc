/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize -frename-registers" } */

#include "riscv_vector.h"

float f (int8_t * restrict in, int8_t * restrict out, int n, int m, unsigned cond, size_t vl, float scalar)
{
  vbool64_t mask = *(vbool64_t*) (in + 1000000);

  for (size_t i = 0; i < n; i++)
    {
      vfloat64m1_t v = __riscv_vle64_v_f64m1 ((double *)(in + i + 200), 3);
      __riscv_vse64_v_f64m1 ((double *)(out + i + 200), v, 3);
      
      vfloat64m1_t v2 = __riscv_vle64_v_f64m1_tumu (mask, v, (double *)(in + i + 300), 3);
      __riscv_vse64_v_f64m1_m (mask, (double *)(out + i + 300), v2, 3);
    }

    vfloat32mf2_t v = __riscv_vfmv_s_f_f32mf2 (scalar, 3);
    *(vfloat32mf2_t*)(out + 100000) = v;
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*3,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */

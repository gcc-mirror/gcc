/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -fno-tree-vectorize -frename-registers" } */

#include "riscv_vector.h"

float f3 (int8_t * restrict in, int8_t * restrict out, int n, int m, unsigned cond, size_t vl, float scalar)
{
  vbool64_t mask = *(vbool64_t*) (in + 1000000);

  for (size_t i = 0; i < n; i++)
    {
      vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + i + 200), vl);
      v = __riscv_vfmv_s_f_f32mf2_tu (v, scalar, 3);
      __riscv_vse32_v_f32mf2 ((float *)(out + i + 200), v, vl);
      
      vfloat32mf2_t v2 = __riscv_vle32_v_f32mf2_tumu (mask, v, (float *)(in + i + 300), vl);
      __riscv_vse32_v_f32mf2_m (mask, (float *)(out + i + 300), v2, vl);
    }
  
  vfloat32m1_t v = *(vfloat32m1_t*)(in + 300000);
  for (size_t i = 0; i < n; i++)
    {
      v = __riscv_vfmv_s_f_f32m1_tu (v, (scalar + i), 3);
    }
  return __riscv_vfmv_f_s_f32m1_f32 (v);
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*3,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*3,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-O1" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 3 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */

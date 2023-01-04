/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, void * restrict mask_in, int n)
{
  vfloat32mf2_t v = __riscv_vle32_v_f32mf2 (in + 10000, 19);
  __riscv_vse32_v_f32mf2 (out + 10000, v, 19);
  for (int i = 0; i < n; i++)
    {
      vint16mf2_t v1 = __riscv_vle16_v_i16mf2 (in + i + 1, 19);
      __riscv_vse16_v_i16mf2 (out + i + 1, v1, 19);
      vint32mf2_t v2 = __riscv_vle32_v_i32mf2 (in + i + 2, 19);
      __riscv_vse32_v_i32mf2 (out + i + 2, v2, 19);
      vbool64_t mask = *(vbool64_t*)mask_in;
      vint32mf2_t v3 = __riscv_vle32_v_i32mf2_tumu (mask, v2, in + i + 200, 13);
      *(vint32mf2_t*)(out + i + 200) = v3;
    }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*13,\s*e32,\s*mf2,\s*tu,\s*mu} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*19,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*19,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 4 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

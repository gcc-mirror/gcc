/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, void * restrict mask_in, int n, int cond)
{
  vbool64_t mask = *(vbool64_t*)mask_in;
  vfloat32mf2_t vf32mf2 = *(vfloat32mf2_t*)in;
  vint16mf4_t vf16mf4 = *(vint16mf4_t*)(in + 5);
  asm volatile ("":::"memory");
  vfloat32mf2_t v = __riscv_vle32_v_f32mf2 (in + 10000, 5);
  __riscv_vse32_v_f32mf2 (out + 10000, v, 5);

  if (cond)
    {
      vfloat32mf2_t vt = __riscv_vle32_v_f32mf2_tumu (mask, vf32mf2, in + 20000, 5);
      __riscv_vse32_v_f32mf2 (out + 20000, vt, 5);
    }
  else
    {
      vint16mf4_t vt = __riscv_vle16_v_i16mf4_tumu (mask, vf16mf4, in + 20000, 5);
      __riscv_vse16_v_i16mf4 (out + 20000, vt, 5);
    }

  for (int i = 0; i < n; i++)
    {
      vfloat32mf2_t v0 = __riscv_vle32_v_f32mf2_tu (v, in + i + 100, 5);
      vint16mf2_t v1 = __riscv_vle16_v_i16mf2 (in + i + 200, 6);
      vint8mf2_t v2 = __riscv_vle8_v_i8mf2 (in + i + 300, 7);
      vint8mf4_t v3 = __riscv_vle8_v_i8mf4 (in + i + 400, 8);
      vint8mf8_t v4 = __riscv_vle8_v_i8mf8 (in + i + 500, 9);
      vint32mf2_t v5 = __riscv_vle32_v_i32mf2 (in + i + 600, 5);
      
      __riscv_vse32_v_f32mf2 (out + i + 100, v0, 5);
      __riscv_vse16_v_i16mf2 (out + i + 200, v1, 6);
      __riscv_vse8_v_i8mf2 (out + i + 300, v2, 7);
      __riscv_vse8_v_i8mf4 (out + i + 400, v3, 8);
      __riscv_vse8_v_i8mf8 (out + i + 500, v4, 9);
      __riscv_vse32_v_i32mf2 (out + i + 600, v5, 5);
    }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*5,\s*e16,\s*mf4,\s*tu,\s*mu} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*5,\s*e32,\s*mf2,\s*tu,\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */


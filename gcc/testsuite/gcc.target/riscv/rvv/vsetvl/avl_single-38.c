/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (int8_t * restrict in, int8_t * restrict out, int8_t * restrict out2, int n, int m, unsigned cond, size_t vl)
{
  vl = 22;
  vbool64_t mask = *(vbool64_t*) (in + 1000000);
  if (cond == 0) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in, vl);
    __riscv_vse8_v_i8mf8 (out, v, vl);
  } else {
    out2[100] = out2[100] + 300;
  }

  for (size_t i = 0; i < n; i++)
    out[i + 200] = out[i + 500] + 22;

  for (size_t i = 0; i < n; i++)
    {
      vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + i + 200), 22);
      __riscv_vse32_v_f32mf2 ((float *)(out + i + 200), v, 22);
      
      vfloat32mf2_t v2 = __riscv_vle32_v_f32mf2_tumu (mask, v, (float *)(in + i + 300), 22);
      __riscv_vse32_v_f32mf2_m (mask, (float *)(out + i + 300), v2, 22);
    }
}

void f2 (int8_t * restrict in, int8_t * restrict out, int n, int m, unsigned cond, size_t vl)
{
  asm volatile ("li %0, 101" :"=r" (vl)::"memory");
  vbool64_t mask = *(vbool64_t*) (in + 1000000);
  if (cond > 0) {
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in, vl);
    __riscv_vse8_v_i8mf8 (out, v, vl);
  } else {
    out[100] = out[100] + 300;
  }

  for (size_t i = 0; i < n; i++)
    out[i + 200] = out[i + 500] + 555;

  for (size_t i = 0; i < n; i++)
    {
      vfloat32mf2_t v = __riscv_vle32_v_f32mf2 ((float *)(in + i + 200), vl);
      __riscv_vse32_v_f32mf2 ((float *)(out + i + 200), v, vl);
      
      vfloat32mf2_t v2 = __riscv_vle32_v_f32mf2_tumu (mask, v, (float *)(in + i + 300), vl);
      __riscv_vse32_v_f32mf2_m (mask, (float *)(out + i + 300), v2, vl);
    }
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,22,\s*e32,\s*mf2,\s*tu,\s*mu} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*mu} 2 { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 4 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli} 2 { target { no-opts "-O0" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */

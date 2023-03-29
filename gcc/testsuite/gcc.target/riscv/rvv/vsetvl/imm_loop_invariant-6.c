/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"


void f8 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vfloat64m1_t v = __riscv_vle64_v_f64m1 (in + i + j, 17);
          __riscv_vse64_v_f64m1 (out + i + j, v, 17);
        }
    }
  }
}

void f9 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vfloat64m2_t v = __riscv_vle64_v_f64m2 (in + i + j, 17);
          __riscv_vse64_v_f64m2 (out + i + j, v, 17);
        }
    }
  }
}

void f11 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vfloat64m4_t v = __riscv_vle64_v_f64m4 (in + i + j, 17);
          __riscv_vse64_v_f64m4 (out + i + j, v, 17);
        }
    }
  }
}

void f13 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vfloat64m8_t v = __riscv_vle64_v_f64m8 (in + i + j, 17);
          __riscv_vse64_v_f64m8 (out + i + j, v, 17);
        }
    }
  }
}


/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle64\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 4 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e64,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e64,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e64,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

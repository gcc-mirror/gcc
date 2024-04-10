/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, void * restrict mask_in, int l, int n, int m)
{
  vbool64_t mask = *(vbool64_t*)mask_in;
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j, 17);
          __riscv_vse8_v_i8mf8 (out + i + j, v, 17);
          vint8mf8_t v2 = __riscv_vle8_v_i8mf8_m (mask, in + i + j + 16, 17);
          __riscv_vse8_v_i8mf8 (out + i + j + 16, v2, 17);
        }
    }
  }
}

void f2 (void * restrict in, void * restrict out, void * restrict mask_in, int l, int n, int m)
{
  vbool8_t mask = *(vbool8_t*)mask_in;
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vfloat64m8_t v = __riscv_vle64_v_f64m8 (in + i + j, 17);
          __riscv_vse64_v_f64m8 (out + i + j, v, 17);
          vfloat64m8_t v2 = __riscv_vle64_v_f64m8_m (mask, in + i + j + 16, 17);
          __riscv_vse64_v_f64m8 (out + i + j + 16, v2, 17);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle8\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle64\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e64,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f5 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint32mf2_t v = __riscv_vle32_v_i32mf2 (in + i + j, 17);
          __riscv_vse32_v_i32mf2 (out + i + j, v, 17);
        }
    }
  }
}

void f6 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint32mf2_t v = __riscv_vle32_v_u32mf2 (in + i + j, 17);
          __riscv_vse32_v_u32mf2 (out + i + j, v, 17);
        }
    }
  }
}

void f7 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint32m1_t v = __riscv_vle32_v_i32m1 (in + i + j, 17);
          __riscv_vse32_v_i32m1 (out + i + j, v, 17);
        }
    }
  }
}

void f8 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint32m1_t v = __riscv_vle32_v_u32m1 (in + i + j, 17);
          __riscv_vse32_v_u32m1 (out + i + j, v, 17);
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
          vint32m2_t v = __riscv_vle32_v_i32m2 (in + i + j, 17);
          __riscv_vse32_v_i32m2 (out + i + j, v, 17);
        }
    }
  }
}

void f10 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint32m2_t v = __riscv_vle32_v_u32m2 (in + i + j, 17);
          __riscv_vse32_v_u32m2 (out + i + j, v, 17);
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
          vint32m4_t v = __riscv_vle32_v_i32m4 (in + i + j, 17);
          __riscv_vse32_v_i32m4 (out + i + j, v, 17);
        }
    }
  }
}

void f12 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint32m4_t v = __riscv_vle32_v_u32m4 (in + i + j, 17);
          __riscv_vse32_v_u32m4 (out + i + j, v, 17);
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
          vint32m8_t v = __riscv_vle32_v_i32m8 (in + i + j, 17);
          __riscv_vse32_v_i32m8 (out + i + j, v, 17);
        }
    }
  }
}

void f14 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint32m8_t v = __riscv_vle32_v_u32m8 (in + i + j, 17);
          __riscv_vse32_v_u32m8 (out + i + j, v, 17);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle32\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 10 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m1,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e32,\s*m8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

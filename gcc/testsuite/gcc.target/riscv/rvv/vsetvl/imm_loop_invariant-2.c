/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f3 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint16mf4_t v = __riscv_vle16_v_i16mf4 (in + i + j, 17);
          __riscv_vse16_v_i16mf4 (out + i + j, v, 17);
        }
    }
  }
}

void f4 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint16mf4_t v = __riscv_vle16_v_u16mf4 (in + i + j, 17);
          __riscv_vse16_v_u16mf4 (out + i + j, v, 17);
        }
    }
  }
}

void f5 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint16mf2_t v = __riscv_vle16_v_i16mf2 (in + i + j, 17);
          __riscv_vse16_v_i16mf2 (out + i + j, v, 17);
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
          vuint16mf2_t v = __riscv_vle16_v_u16mf2 (in + i + j, 17);
          __riscv_vse16_v_u16mf2 (out + i + j, v, 17);
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
          vint16m1_t v = __riscv_vle16_v_i16m1 (in + i + j, 17);
          __riscv_vse16_v_i16m1 (out + i + j, v, 17);
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
          vuint16m1_t v = __riscv_vle16_v_u16m1 (in + i + j, 17);
          __riscv_vse16_v_u16m1 (out + i + j, v, 17);
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
          vint16m2_t v = __riscv_vle16_v_i16m2 (in + i + j, 17);
          __riscv_vse16_v_i16m2 (out + i + j, v, 17);
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
          vuint16m2_t v = __riscv_vle16_v_u16m2 (in + i + j, 17);
          __riscv_vse16_v_u16m2 (out + i + j, v, 17);
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
          vint16m4_t v = __riscv_vle16_v_i16m4 (in + i + j, 17);
          __riscv_vse16_v_i16m4 (out + i + j, v, 17);
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
          vuint16m4_t v = __riscv_vle16_v_u16m4 (in + i + j, 17);
          __riscv_vse16_v_u16m4 (out + i + j, v, 17);
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
          vint16m8_t v = __riscv_vle16_v_i16m8 (in + i + j, 17);
          __riscv_vse16_v_i16m8 (out + i + j, v, 17);
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
          vuint16m8_t v = __riscv_vle16_v_u16m8 (in + i + j, 17);
          __riscv_vse16_v_u16m8 (out + i + j, v, 17);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle16\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 12 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e16,\s*m1,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e16,\s*m2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e16,\s*m4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e16,\s*m8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

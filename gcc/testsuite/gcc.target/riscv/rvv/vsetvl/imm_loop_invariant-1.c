/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void f1 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf8_t v = __riscv_vle8_v_i8mf8 (in + i + j, 17);
          __riscv_vse8_v_i8mf8 (out + i + j, v, 17);
        }
    }
  }
}

void f2 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vuint8mf8_t v = __riscv_vle8_v_u8mf8 (in + i + j, 17);
          __riscv_vse8_v_u8mf8 (out + i + j, v, 17);
        }
    }
  }
}

void f3 (void * restrict in, void * restrict out, int l, int n, int m)
{
  for (int i = 0; i < l; i++){
    for (int j = 0; j < m; j++){
      for (int k = 0; k < n; k++)
        {
          vint8mf4_t v = __riscv_vle8_v_i8mf4 (in + i + j, 17);
          __riscv_vse8_v_i8mf4 (out + i + j, v, 17);
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
          vuint8mf4_t v = __riscv_vle8_v_u8mf4 (in + i + j, 17);
          __riscv_vse8_v_u8mf4 (out + i + j, v, 17);
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
          vint8mf2_t v = __riscv_vle8_v_i8mf2 (in + i + j, 17);
          __riscv_vse8_v_i8mf2 (out + i + j, v, 17);
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
          vuint8mf2_t v = __riscv_vle8_v_u8mf2 (in + i + j, 17);
          __riscv_vse8_v_u8mf2 (out + i + j, v, 17);
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
          vint8m1_t v = __riscv_vle8_v_i8m1 (in + i + j, 17);
          __riscv_vse8_v_i8m1 (out + i + j, v, 17);
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
          vuint8m1_t v = __riscv_vle8_v_u8m1 (in + i + j, 17);
          __riscv_vse8_v_u8m1 (out + i + j, v, 17);
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
          vint8m2_t v = __riscv_vle8_v_i8m2 (in + i + j, 17);
          __riscv_vse8_v_i8m2 (out + i + j, v, 17);
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
          vuint8m2_t v = __riscv_vle8_v_u8m2 (in + i + j, 17);
          __riscv_vse8_v_u8m2 (out + i + j, v, 17);
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
          vint8m4_t v = __riscv_vle8_v_i8m4 (in + i + j, 17);
          __riscv_vse8_v_i8m4 (out + i + j, v, 17);
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
          vuint8m4_t v = __riscv_vle8_v_u8m4 (in + i + j, 17);
          __riscv_vse8_v_u8m4 (out + i + j, v, 17);
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
          vint8m8_t v = __riscv_vle8_v_i8m8 (in + i + j, 17);
          __riscv_vse8_v_i8m8 (out + i + j, v, 17);
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
          vuint8m8_t v = __riscv_vle8_v_u8m8 (in + i + j, 17);
          __riscv_vse8_v_u8m8 (out + i + j, v, 17);
        }
    }
  }
}

/* { dg-final { scan-assembler-times {\.L[0-9]+\:\s+vle8\.v\s+v[0-9]+,\s*0\s*\([a-x0-9]+\)} 14 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m1,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m4,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*17,\s*e8,\s*m8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-g" no-opts "-funroll-loops" } } } } */

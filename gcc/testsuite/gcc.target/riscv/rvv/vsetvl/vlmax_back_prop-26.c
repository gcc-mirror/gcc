/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int n, int cond)
{
  if (cond == 0) {
    vint8mf8_t v = *(vint8mf8_t*)(in + 100);
    *(vint8mf8_t*)(out + 100) = v;
    for (int i = 0; i < n; i++)
      {
        vint16mf4_t v2;
        *(vint16mf4_t*)(out + i + 100) = v2;
      }
  } else if (cond == 1) {
    vint8mf8_t v = *(vint8mf8_t*)(in + 200);
    *(vint8mf8_t*)(out + 200) = v;
    for (int i = 0; i < n; i++)
      {
        vint32mf2_t v2;
        *(vint32mf2_t*)(out + i + 200) = v2;
      }
  } else if (cond == 2) {
    vint8mf8_t v = *(vint8mf8_t*)(in + 300);
    *(vint8mf8_t*)(out + 300) = v;
    for (int i = 0; i < n; i++)
      {
        vint8mf8_t v2;
        *(vint8mf8_t*)(out + i + 300) = v2;
      }
  } else if (cond == 3) {
    vint8mf8_t v = *(vint8mf8_t*)(in + 400);
    *(vint8mf8_t*)(out + 400) = v;
    for (int i = 0; i < n; i++)
      {
        vint64m1_t v2;
        *(vint64m1_t*)(out + i + 400) = v2;
      }
  } else if (cond == 4) {
    vint8mf8_t v = *(vint8mf8_t*)(in + 500);
    *(vint8mf8_t*)(out + 500) = v;
    for (int i = 0; i < n; i++)
      {
        vfloat32mf2_t v2;
        *(vfloat32mf2_t*)(out + i + 500) = v2;
      }
  } else if (cond == 5) {
    vuint8mf8_t v = *(vuint8mf8_t*)(in + 600);
    *(vuint8mf8_t*)(out + 600) = v;
    for (int i = 0; i < n; i++)
      {
        vuint16mf4_t v2;
        *(vuint16mf4_t*)(out + i + 600) = v2;
      }
  } else if (cond == 6) {
    vuint8mf8_t v = *(vuint8mf8_t*)(in + 700);
    *(vuint8mf8_t*)(out + 700) = v;
    for (int i = 0; i < n; i++)
      {
        vuint32mf2_t v2;
        *(vuint32mf2_t*)(out + i + 700) = v2;
      }
  } else if (cond == 7) {
    vuint8mf8_t v = *(vuint8mf8_t*)(in + 800);
    *(vuint8mf8_t*)(out + 800) = v;
    for (int i = 0; i < n; i++)
      {
        vuint8mf8_t v2;
        *(vuint8mf8_t*)(out + i + 800) = v2;
      }
  } else if (cond == 8) {
    vuint8mf8_t v = *(vuint8mf8_t*)(in + 900);
    *(vuint8mf8_t*)(out + 900) = v;
    for (int i = 0; i < n; i++)
      {
        vuint64m1_t v2;
        *(vuint64m1_t*)(out + i + 900) = v2;
      }
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 8 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 17 { target { no-opts "-O0"  no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

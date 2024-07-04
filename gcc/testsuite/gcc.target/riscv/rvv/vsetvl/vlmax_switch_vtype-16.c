/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

void f (void * restrict in, void * restrict out, int32_t * a, int32_t * b, int n, int cond)
{
  for (int i = 0; i < n; i++) {
    vint16mf4_t v;
    *(vint16mf4_t*)(out + i + 700) = v;
  }
  for (int i = 0; i < n; i++) {
    a[i] = a[i] + b[i];
  }
  for (int i = 0; i < n; i++) {
    a[i] = a[i] * b[i];
  }
  for (int i = 0; i < n; i++) {
    a[i] = a[i] - b[i];
  }
  for (int i = 0; i < n; i++) {
    vint32mf2_t v;
    *(vint32mf2_t*)(out + i + 7000) = v;
  }
  for (int i = 0; i < n; i++) {
    vint16mf2_t v;
    *(vint16mf2_t*)(out + i + 777) = v;
  }
  for (int i = 0; i < n; i++) {
    vint64m1_t v;
    *(vint64m1_t*)(out + i + 8000) = v;
  }
  if (cond == 0) {
    vbool64_t v;
    *(vbool64_t*)(out + 1234) = v;
  } else {
    vuint8mf8_t v;
    *(vuint8mf8_t*)(out + 5432) = v;
  }
  for (int i = 0; i < n; i++) {
    vfloat32mf2_t v;
    *(vfloat32mf2_t*)(out + i + 7777) = v;
  }
  for (int i = 0; i < n; i++) {
    vuint16mf2_t v;
    *(vuint16mf2_t*)(out + i + 888) = v;
  }
  for (int i = 0; i < n; i++) {
    vint8mf8_t v;
    *(vint8mf8_t*)(out + i + 9000) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero} 3 { target { no-opts "-O0" no-opts "-funroll-loops" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 2 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 3 { target { no-opts "-O0"  no-opts "-funroll-loops" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli} 10 { target { no-opts "-O0" no-opts "-O1" no-opts "-funroll-loops" no-opts "-Os" no-opts "-Oz" no-opts "-flto" no-opts "-g" } } } } */

/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void fn3 (void);

void f6 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 900 + i);
    *(vfloat32mf2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 900 + i);
    *(vfloat32mf2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 900 + i);
    *(vfloat32mf2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 900 + i);
    *(vfloat32mf2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vfloat32mf2_t v = *(vfloat32mf2_t*)(in + 900 + i);
    *(vfloat32mf2_t*)(out + 900 + i) = v;
  }
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9]\:\s+vle32\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

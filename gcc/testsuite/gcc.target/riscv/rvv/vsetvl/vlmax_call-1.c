/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32 -O3 -fno-tree-vectorize" } */

#include "riscv_vector.h"

void fn3 (void);

void f (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool64_t v = *(vbool64_t*)(in + 900 + i);
    *(vbool64_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool64_t v = *(vbool64_t*)(in + 900 + i);
    *(vbool64_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool64_t v = *(vbool64_t*)(in + 900 + i);
    *(vbool64_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool64_t v = *(vbool64_t*)(in + 900 + i);
    *(vbool64_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool64_t v = *(vbool64_t*)(in + 900 + i);
    *(vbool64_t*)(out + 900 + i) = v;
  }

}

int f2 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool32_t v = *(vbool32_t*)(in + 900 + i);
    *(vbool32_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool32_t v = *(vbool32_t*)(in + 900 + i);
    *(vbool32_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool32_t v = *(vbool32_t*)(in + 900 + i);
    *(vbool32_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool32_t v = *(vbool32_t*)(in + 900 + i);
    *(vbool32_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool32_t v = *(vbool32_t*)(in + 900 + i);
    *(vbool32_t*)(out + 900 + i) = v;
  }

}

int f3 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool16_t v = *(vbool16_t*)(in + 900 + i);
    *(vbool16_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool16_t v = *(vbool16_t*)(in + 900 + i);
    *(vbool16_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool16_t v = *(vbool16_t*)(in + 900 + i);
    *(vbool16_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool16_t v = *(vbool16_t*)(in + 900 + i);
    *(vbool16_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool16_t v = *(vbool16_t*)(in + 900 + i);
    *(vbool16_t*)(out + 900 + i) = v;
  }

}

int f4 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool8_t v = *(vbool8_t*)(in + 900 + i);
    *(vbool8_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool8_t v = *(vbool8_t*)(in + 900 + i);
    *(vbool8_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool8_t v = *(vbool8_t*)(in + 900 + i);
    *(vbool8_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool8_t v = *(vbool8_t*)(in + 900 + i);
    *(vbool8_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool8_t v = *(vbool8_t*)(in + 900 + i);
    *(vbool8_t*)(out + 900 + i) = v;
  }

}

int f5 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool4_t v = *(vbool4_t*)(in + 900 + i);
    *(vbool4_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool4_t v = *(vbool4_t*)(in + 900 + i);
    *(vbool4_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool4_t v = *(vbool4_t*)(in + 900 + i);
    *(vbool4_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool4_t v = *(vbool4_t*)(in + 900 + i);
    *(vbool4_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool4_t v = *(vbool4_t*)(in + 900 + i);
    *(vbool4_t*)(out + 900 + i) = v;
  }

}

int f6 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool2_t v = *(vbool2_t*)(in + 900 + i);
    *(vbool2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool2_t v = *(vbool2_t*)(in + 900 + i);
    *(vbool2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool2_t v = *(vbool2_t*)(in + 900 + i);
    *(vbool2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool2_t v = *(vbool2_t*)(in + 900 + i);
    *(vbool2_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool2_t v = *(vbool2_t*)(in + 900 + i);
    *(vbool2_t*)(out + 900 + i) = v;
  }

}

int f7 (int8_t * restrict in, int8_t * restrict out, int n)
{
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool1_t v = *(vbool1_t*)(in + 900 + i);
    *(vbool1_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool1_t v = *(vbool1_t*)(in + 900 + i);
    *(vbool1_t*)(out + 900 + i) = v;
  }
  fn3 ();
  
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool1_t v = *(vbool1_t*)(in + 900 + i);
    *(vbool1_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool1_t v = *(vbool1_t*)(in + 900 + i);
    *(vbool1_t*)(out + 900 + i) = v;
  }
  fn3 ();
  for (int i = 0 ; i < n * n * n * n; i++) {
    vbool1_t v = *(vbool1_t*)(in + 900 + i);
    *(vbool1_t*)(out + 900 + i) = v;
  }

}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m1,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m4,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+\.L[0-9]+\:\s+vlm\.v\s+v[0-9]+,0\s*\([a-x0-9]+\)} 5 { target { no-opts "-O0"  no-opts "-Os" no-opts "-Oz" no-opts "-funroll-loops" no-opts "-g" } } } } */

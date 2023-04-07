/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

void foo7 (void * restrict in, void * restrict out)
{
  vbool1_t v1 = *(vbool1_t*)(in + 1);
  vbool2_t v2 = *(vbool2_t*)(in + 2);
  vbool4_t v3 = *(vbool4_t*)(in + 3);
  vbool8_t v4 = *(vbool8_t*)(in + 4);
  vbool16_t v5 = *(vbool16_t*)(in + 5);
  vbool32_t v6 = *(vbool32_t*)(in + 6);
  vbool64_t v7 = *(vbool64_t*)(in + 7);
  *(vbool1_t*)(out + 1) = v1;
  *(vbool2_t*)(out + 2) = v2;
  *(vbool4_t*)(out + 3) = v3;
  *(vbool8_t*)(out + 4) = v4;
  *(vbool16_t*)(out + 5) = v5;
  *(vbool32_t*)(out + 6) = v6;
  *(vbool64_t*)(out + 7) = v7;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vlm\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 7 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */
/* { dg-final { scan-assembler-times {vsm\.v\s+(?:v[0-9]|v[1-2][0-9]|v3[0-1]),0\s*\([a-x0-9]+\)} 7 { target { no-opts "-O0" no-opts "-O1"  no-opts "-Os" no-opts "-Oz" no-opts "-Og -g" } } } } */

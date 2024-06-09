/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv -mabi=ilp32" } */

/* Test insert-vsetvl PASS whether it's able to eliminate vsetvl for same vtype in VLMAX.  */

#include "riscv_vector.h"

void foo1 (void * restrict in, void * restrict out)
{
  vbool64_t v1 = *(vbool64_t*)(in + 1);
  vbool64_t v2 = *(vbool64_t*)(in + 2);
  vbool64_t v3 = *(vbool64_t*)(in + 3);
  vbool64_t v4 = *(vbool64_t*)(in + 4);
  vbool64_t v5 = *(vbool64_t*)(in + 5);
  vbool64_t v6 = *(vbool64_t*)(in + 6);
  vbool64_t v7 = *(vbool64_t*)(in + 7);
  vbool64_t v8 = *(vbool64_t*)(in + 8);
  *(vbool64_t*)(out + 1) = v1;
  *(vbool64_t*)(out + 2) = v2;
  *(vbool64_t*)(out + 3) = v3;
  *(vbool64_t*)(out + 4) = v4;
  *(vbool64_t*)(out + 5) = v5;
  *(vbool64_t*)(out + 6) = v6;
  *(vbool64_t*)(out + 7) = v7;
  *(vbool64_t*)(out + 8) = v8;
}

void foo2 (void * restrict in, void * restrict out)
{
  vbool32_t v1 = *(vbool32_t*)(in + 1);
  vbool32_t v2 = *(vbool32_t*)(in + 2);
  vbool32_t v3 = *(vbool32_t*)(in + 3);
  vbool32_t v4 = *(vbool32_t*)(in + 4);
  vbool32_t v5 = *(vbool32_t*)(in + 5);
  vbool32_t v6 = *(vbool32_t*)(in + 6);
  vbool32_t v7 = *(vbool32_t*)(in + 7);
  vbool32_t v8 = *(vbool32_t*)(in + 8);
  *(vbool32_t*)(out + 1) = v1;
  *(vbool32_t*)(out + 2) = v2;
  *(vbool32_t*)(out + 3) = v3;
  *(vbool32_t*)(out + 4) = v4;
  *(vbool32_t*)(out + 5) = v5;
  *(vbool32_t*)(out + 6) = v6;
  *(vbool32_t*)(out + 7) = v7;
  *(vbool32_t*)(out + 8) = v8;
}

void foo3 (void * restrict in, void * restrict out)
{
  vbool16_t v1 = *(vbool16_t*)(in + 1);
  vbool16_t v2 = *(vbool16_t*)(in + 2);
  vbool16_t v3 = *(vbool16_t*)(in + 3);
  vbool16_t v4 = *(vbool16_t*)(in + 4);
  vbool16_t v5 = *(vbool16_t*)(in + 5);
  vbool16_t v6 = *(vbool16_t*)(in + 6);
  vbool16_t v7 = *(vbool16_t*)(in + 7);
  vbool16_t v8 = *(vbool16_t*)(in + 8);
  *(vbool16_t*)(out + 1) = v1;
  *(vbool16_t*)(out + 2) = v2;
  *(vbool16_t*)(out + 3) = v3;
  *(vbool16_t*)(out + 4) = v4;
  *(vbool16_t*)(out + 5) = v5;
  *(vbool16_t*)(out + 6) = v6;
  *(vbool16_t*)(out + 7) = v7;
  *(vbool16_t*)(out + 8) = v8;
}

void foo4 (void * restrict in, void * restrict out)
{
  vbool8_t v1 = *(vbool8_t*)(in + 1);
  vbool8_t v2 = *(vbool8_t*)(in + 2);
  vbool8_t v3 = *(vbool8_t*)(in + 3);
  vbool8_t v4 = *(vbool8_t*)(in + 4);
  vbool8_t v5 = *(vbool8_t*)(in + 5);
  vbool8_t v6 = *(vbool8_t*)(in + 6);
  vbool8_t v7 = *(vbool8_t*)(in + 7);
  vbool8_t v8 = *(vbool8_t*)(in + 8);
  *(vbool8_t*)(out + 1) = v1;
  *(vbool8_t*)(out + 2) = v2;
  *(vbool8_t*)(out + 3) = v3;
  *(vbool8_t*)(out + 4) = v4;
  *(vbool8_t*)(out + 5) = v5;
  *(vbool8_t*)(out + 6) = v6;
  *(vbool8_t*)(out + 7) = v7;
  *(vbool8_t*)(out + 8) = v8;
}

void foo5 (void * restrict in, void * restrict out)
{
  vbool4_t v1 = *(vbool4_t*)(in + 1);
  vbool4_t v2 = *(vbool4_t*)(in + 2);
  vbool4_t v3 = *(vbool4_t*)(in + 3);
  vbool4_t v4 = *(vbool4_t*)(in + 4);
  vbool4_t v5 = *(vbool4_t*)(in + 5);
  vbool4_t v6 = *(vbool4_t*)(in + 6);
  vbool4_t v7 = *(vbool4_t*)(in + 7);
  vbool4_t v8 = *(vbool4_t*)(in + 8);
  *(vbool4_t*)(out + 1) = v1;
  *(vbool4_t*)(out + 2) = v2;
  *(vbool4_t*)(out + 3) = v3;
  *(vbool4_t*)(out + 4) = v4;
  *(vbool4_t*)(out + 5) = v5;
  *(vbool4_t*)(out + 6) = v6;
  *(vbool4_t*)(out + 7) = v7;
  *(vbool4_t*)(out + 8) = v8;
}

void foo6 (void * restrict in, void * restrict out)
{
  vbool2_t v1 = *(vbool2_t*)(in + 1);
  vbool2_t v2 = *(vbool2_t*)(in + 2);
  vbool2_t v3 = *(vbool2_t*)(in + 3);
  vbool2_t v4 = *(vbool2_t*)(in + 4);
  vbool2_t v5 = *(vbool2_t*)(in + 5);
  vbool2_t v6 = *(vbool2_t*)(in + 6);
  vbool2_t v7 = *(vbool2_t*)(in + 7);
  vbool2_t v8 = *(vbool2_t*)(in + 8);
  *(vbool2_t*)(out + 1) = v1;
  *(vbool2_t*)(out + 2) = v2;
  *(vbool2_t*)(out + 3) = v3;
  *(vbool2_t*)(out + 4) = v4;
  *(vbool2_t*)(out + 5) = v5;
  *(vbool2_t*)(out + 6) = v6;
  *(vbool2_t*)(out + 7) = v7;
  *(vbool2_t*)(out + 8) = v8;
}

void foo7 (void * restrict in, void * restrict out)
{
  vbool1_t v1 = *(vbool1_t*)(in + 1);
  vbool1_t v2 = *(vbool1_t*)(in + 2);
  vbool1_t v3 = *(vbool1_t*)(in + 3);
  vbool1_t v4 = *(vbool1_t*)(in + 4);
  vbool1_t v5 = *(vbool1_t*)(in + 5);
  vbool1_t v6 = *(vbool1_t*)(in + 6);
  vbool1_t v7 = *(vbool1_t*)(in + 7);
  vbool1_t v8 = *(vbool1_t*)(in + 8);
  *(vbool1_t*)(out + 1) = v1;
  *(vbool1_t*)(out + 2) = v2;
  *(vbool1_t*)(out + 3) = v3;
  *(vbool1_t*)(out + 4) = v4;
  *(vbool1_t*)(out + 5) = v5;
  *(vbool1_t*)(out + 6) = v6;
  *(vbool1_t*)(out + 7) = v7;
  *(vbool1_t*)(out + 8) = v8;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */

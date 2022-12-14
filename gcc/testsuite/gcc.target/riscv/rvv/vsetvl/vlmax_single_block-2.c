/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

/* Test insert-vsetvl PASS whether it's able to eliminate vsetvl for same vtype in VLMAX.  */

#include "riscv_vector.h"

void foo1 (void * restrict in, void * restrict out)
{
  vint8mf8_t v1 = *(vint8mf8_t*)(in + 1);
  vint8mf8_t v2 = *(vint8mf8_t*)(in + 2);
  vint8mf8_t v3 = *(vint8mf8_t*)(in + 3);
  vint8mf8_t v4 = *(vint8mf8_t*)(in + 4);
  vint8mf8_t v5 = *(vint8mf8_t*)(in + 5);
  vint8mf8_t v6 = *(vint8mf8_t*)(in + 6);
  vint8mf8_t v7 = *(vint8mf8_t*)(in + 7);
  vint8mf8_t v8 = *(vint8mf8_t*)(in + 8);
  *(vint8mf8_t*)(out + 1) = v1;
  *(vint8mf8_t*)(out + 2) = v2;
  *(vint8mf8_t*)(out + 3) = v3;
  *(vint8mf8_t*)(out + 4) = v4;
  *(vint8mf8_t*)(out + 5) = v5;
  *(vint8mf8_t*)(out + 6) = v6;
  *(vint8mf8_t*)(out + 7) = v7;
  *(vint8mf8_t*)(out + 8) = v8;
}

void foo2 (void * restrict in, void * restrict out)
{
  vint8mf4_t v1 = *(vint8mf4_t*)(in + 1);
  vint8mf4_t v2 = *(vint8mf4_t*)(in + 2);
  vint8mf4_t v3 = *(vint8mf4_t*)(in + 3);
  vint8mf4_t v4 = *(vint8mf4_t*)(in + 4);
  vint8mf4_t v5 = *(vint8mf4_t*)(in + 5);
  vint8mf4_t v6 = *(vint8mf4_t*)(in + 6);
  vint8mf4_t v7 = *(vint8mf4_t*)(in + 7);
  vint8mf4_t v8 = *(vint8mf4_t*)(in + 8);
  *(vint8mf4_t*)(out + 1) = v1;
  *(vint8mf4_t*)(out + 2) = v2;
  *(vint8mf4_t*)(out + 3) = v3;
  *(vint8mf4_t*)(out + 4) = v4;
  *(vint8mf4_t*)(out + 5) = v5;
  *(vint8mf4_t*)(out + 6) = v6;
  *(vint8mf4_t*)(out + 7) = v7;
  *(vint8mf4_t*)(out + 8) = v8;
}

void foo3 (void * restrict in, void * restrict out)
{
  vint8mf2_t v1 = *(vint8mf2_t*)(in + 1);
  vint8mf2_t v2 = *(vint8mf2_t*)(in + 2);
  vint8mf2_t v3 = *(vint8mf2_t*)(in + 3);
  vint8mf2_t v4 = *(vint8mf2_t*)(in + 4);
  vint8mf2_t v5 = *(vint8mf2_t*)(in + 5);
  vint8mf2_t v6 = *(vint8mf2_t*)(in + 6);
  vint8mf2_t v7 = *(vint8mf2_t*)(in + 7);
  vint8mf2_t v8 = *(vint8mf2_t*)(in + 8);
  *(vint8mf2_t*)(out + 1) = v1;
  *(vint8mf2_t*)(out + 2) = v2;
  *(vint8mf2_t*)(out + 3) = v3;
  *(vint8mf2_t*)(out + 4) = v4;
  *(vint8mf2_t*)(out + 5) = v5;
  *(vint8mf2_t*)(out + 6) = v6;
  *(vint8mf2_t*)(out + 7) = v7;
  *(vint8mf2_t*)(out + 8) = v8;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf2,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */

/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32" } */

#include "riscv_vector.h"

void f1 (void * restrict in, void * restrict out)
{
  vint32mf2_t v1 = *(vint32mf2_t*)(in + 1);
  vint8mf8_t v2;
  *(vint32mf2_t*)(out + 1) = v1;
  *(vint8mf8_t*)(out + 2) = v2;
}

void f2 (void * restrict in, void * restrict out)
{
  vint32mf2_t v1 = *(vint32mf2_t*)(in + 1);
  vint16mf4_t v2;
  *(vint32mf2_t*)(out + 1) = v1;
  *(vint16mf4_t*)(out + 2) = v2;
}

void f3 (void * restrict in, void * restrict out)
{
  vint32mf2_t v1 = *(vint32mf2_t*)(in + 1);
  vint64m1_t v2;
  *(vint32mf2_t*)(out + 1) = v1;
  *(vint64m1_t*)(out + 2) = v2;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e64,\s*m1,\s*t[au],\s*m[au]} 1 { target { no-opts "-O0" } } } } */

/* { dg-final { scan-assembler-times {vsetvli} 3 { target { no-opts "-O0" } } } } */

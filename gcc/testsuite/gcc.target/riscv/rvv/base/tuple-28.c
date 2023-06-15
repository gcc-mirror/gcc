/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv_zvfh -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vfloat16mf4x2_t (void *base, void *out)
{
  vfloat16mf4x2_t v = *(vfloat16mf4x2_t*)base;
  *(vfloat16mf4x2_t*)out = v;
}

void
f_vfloat16mf4x3_t (void *base, void *out)
{
  vfloat16mf4x3_t v = *(vfloat16mf4x3_t*)base;
  *(vfloat16mf4x3_t*)out = v;
}

void
f_vfloat16mf4x4_t (void *base, void *out)
{
  vfloat16mf4x4_t v = *(vfloat16mf4x4_t*)base;
  *(vfloat16mf4x4_t*)out = v;
}

void
f_vfloat16mf4x5_t (void *base, void *out)
{
  vfloat16mf4x5_t v = *(vfloat16mf4x5_t*)base;
  *(vfloat16mf4x5_t*)out = v;
}

void
f_vfloat16mf4x6_t (void *base, void *out)
{
  vfloat16mf4x6_t v = *(vfloat16mf4x6_t*)base;
  *(vfloat16mf4x6_t*)out = v;
}

void
f_vfloat16mf4x7_t (void *base, void *out)
{
  vfloat16mf4x7_t v = *(vfloat16mf4x7_t*)base;
  *(vfloat16mf4x7_t*)out = v;
}

void
f_vfloat16mf4x8_t (void *base, void *out)
{
  vfloat16mf4x8_t v = *(vfloat16mf4x8_t*)base;
  *(vfloat16mf4x8_t*)out = v;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 7 } } */
/* { dg-final { scan-assembler {srai} } } */
/* { dg-final { scan-assembler-not {slli} } } */
/* { dg-final { scan-assembler-times {vle16\.v\tv[0-9]+,0\([a-x0-9]+\)} 35 } } */
/* { dg-final { scan-assembler-times {vse16\.v\tv[0-9]+,0\([a-x0-9]+\)} 35 } } */
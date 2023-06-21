/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vfloat32mf2x2_t (void *base, void *out)
{
  vfloat32mf2x2_t v = *(vfloat32mf2x2_t*)base;
  *(vfloat32mf2x2_t*)out = v;
}

void
f_vfloat32mf2x3_t (void *base, void *out)
{
  vfloat32mf2x3_t v = *(vfloat32mf2x3_t*)base;
  *(vfloat32mf2x3_t*)out = v;
}

void
f_vfloat32mf2x4_t (void *base, void *out)
{
  vfloat32mf2x4_t v = *(vfloat32mf2x4_t*)base;
  *(vfloat32mf2x4_t*)out = v;
}

void
f_vfloat32mf2x5_t (void *base, void *out)
{
  vfloat32mf2x5_t v = *(vfloat32mf2x5_t*)base;
  *(vfloat32mf2x5_t*)out = v;
}

void
f_vfloat32mf2x6_t (void *base, void *out)
{
  vfloat32mf2x6_t v = *(vfloat32mf2x6_t*)base;
  *(vfloat32mf2x6_t*)out = v;
}

void
f_vfloat32mf2x7_t (void *base, void *out)
{
  vfloat32mf2x7_t v = *(vfloat32mf2x7_t*)base;
  *(vfloat32mf2x7_t*)out = v;
}

void
f_vfloat32mf2x8_t (void *base, void *out)
{
  vfloat32mf2x8_t v = *(vfloat32mf2x8_t*)base;
  *(vfloat32mf2x8_t*)out = v;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e32,\s*mf2,\s*t[au],\s*m[au]} 7 } } */
/* { dg-final { scan-assembler {srai} } } */
/* { dg-final { scan-assembler-not {slli} } } */
/* { dg-final { scan-assembler-times {vle32\.v\tv[0-9]+,0\([a-x0-9]+\)} 35 } } */
/* { dg-final { scan-assembler-times {vse32\.v\tv[0-9]+,0\([a-x0-9]+\)} 35 } } */

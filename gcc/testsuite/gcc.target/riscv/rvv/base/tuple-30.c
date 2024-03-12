/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv_zvfh -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vfloat16m1x2_t (void *base, void *out)
{
  vfloat16m1x2_t v = *(vfloat16m1x2_t*)base;
  *(vfloat16m1x2_t*)out = v;
}

void
f_vfloat16m1x3_t (void *base, void *out)
{
  vfloat16m1x3_t v = *(vfloat16m1x3_t*)base;
  *(vfloat16m1x3_t*)out = v;
}

void
f_vfloat16m1x4_t (void *base, void *out)
{
  vfloat16m1x4_t v = *(vfloat16m1x4_t*)base;
  *(vfloat16m1x4_t*)out = v;
}

void
f_vfloat16m1x5_t (void *base, void *out)
{
  vfloat16m1x5_t v = *(vfloat16m1x5_t*)base;
  *(vfloat16m1x5_t*)out = v;
}

void
f_vfloat16m1x6_t (void *base, void *out)
{
  vfloat16m1x6_t v = *(vfloat16m1x6_t*)base;
  *(vfloat16m1x6_t*)out = v;
}

void
f_vfloat16m1x7_t (void *base, void *out)
{
  vfloat16m1x7_t v = *(vfloat16m1x7_t*)base;
  *(vfloat16m1x7_t*)out = v;
}

void
f_vfloat16m1x8_t (void *base, void *out)
{
  vfloat16m1x8_t v = *(vfloat16m1x8_t*)base;
  *(vfloat16m1x8_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler-not {slli} } } */
/* { dg-final { scan-assembler-times {vl1re16\.v\tv[0-9]+,0\([a-x0-9]+\)} 35 } } */
/* { dg-final { scan-assembler-times {vs1r\.v\tv[0-9]+,0\([a-x0-9]+\)} 35 } } */

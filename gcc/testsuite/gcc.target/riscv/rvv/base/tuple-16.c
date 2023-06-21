/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vint64m1x2_t (void *base, void *out)
{
  vint64m1x2_t v = *(vint64m1x2_t*)base;
  *(vint64m1x2_t*)out = v;
}

void
f_vuint64m1x2_t (void *base, void *out)
{
  vuint64m1x2_t v = *(vuint64m1x2_t*)base;
  *(vuint64m1x2_t*)out = v;
}

void
f_vint64m1x3_t (void *base, void *out)
{
  vint64m1x3_t v = *(vint64m1x3_t*)base;
  *(vint64m1x3_t*)out = v;
}

void
f_vuint64m1x3_t (void *base, void *out)
{
  vuint64m1x3_t v = *(vuint64m1x3_t*)base;
  *(vuint64m1x3_t*)out = v;
}

void
f_vint64m1x4_t (void *base, void *out)
{
  vint64m1x4_t v = *(vint64m1x4_t*)base;
  *(vint64m1x4_t*)out = v;
}

void
f_vuint64m1x4_t (void *base, void *out)
{
  vuint64m1x4_t v = *(vuint64m1x4_t*)base;
  *(vuint64m1x4_t*)out = v;
}

void
f_vint64m1x5_t (void *base, void *out)
{
  vint64m1x5_t v = *(vint64m1x5_t*)base;
  *(vint64m1x5_t*)out = v;
}

void
f_vuint64m1x5_t (void *base, void *out)
{
  vuint64m1x5_t v = *(vuint64m1x5_t*)base;
  *(vuint64m1x5_t*)out = v;
}

void
f_vint64m1x6_t (void *base, void *out)
{
  vint64m1x6_t v = *(vint64m1x6_t*)base;
  *(vint64m1x6_t*)out = v;
}

void
f_vuint64m1x6_t (void *base, void *out)
{
  vuint64m1x6_t v = *(vuint64m1x6_t*)base;
  *(vuint64m1x6_t*)out = v;
}

void
f_vint64m1x7_t (void *base, void *out)
{
  vint64m1x7_t v = *(vint64m1x7_t*)base;
  *(vint64m1x7_t*)out = v;
}

void
f_vuint64m1x7_t (void *base, void *out)
{
  vuint64m1x7_t v = *(vuint64m1x7_t*)base;
  *(vuint64m1x7_t*)out = v;
}

void
f_vint64m1x8_t (void *base, void *out)
{
  vint64m1x8_t v = *(vint64m1x8_t*)base;
  *(vint64m1x8_t*)out = v;
}

void
f_vuint64m1x8_t (void *base, void *out)
{
  vuint64m1x8_t v = *(vuint64m1x8_t*)base;
  *(vuint64m1x8_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler-not {slli} } } */
/* { dg-final { scan-assembler-times {vl1re64\.v\tv[0-9]+,0\([a-x0-9]+\)} 70 } } */
/* { dg-final { scan-assembler-times {vs1r\.v\tv[0-9]+,0\([a-x0-9]+\)} 70 } } */

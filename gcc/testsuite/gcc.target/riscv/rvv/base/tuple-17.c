/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d" } */

#include "riscv_vector.h"

void
f_vint64m2x2_t (void *base, void *out)
{
  vint64m2x2_t v = *(vint64m2x2_t*)base;
  *(vint64m2x2_t*)out = v;
}

void
f_vuint64m2x2_t (void *base, void *out)
{
  vuint64m2x2_t v = *(vuint64m2x2_t*)base;
  *(vuint64m2x2_t*)out = v;
}

void
f_vint64m2x3_t (void *base, void *out)
{
  vint64m2x3_t v = *(vint64m2x3_t*)base;
  *(vint64m2x3_t*)out = v;
}

void
f_vuint64m2x3_t (void *base, void *out)
{
  vuint64m2x3_t v = *(vuint64m2x3_t*)base;
  *(vuint64m2x3_t*)out = v;
}

void
f_vint64m2x4_t (void *base, void *out)
{
  vint64m2x4_t v = *(vint64m2x4_t*)base;
  *(vint64m2x4_t*)out = v;
}

void
f_vuint64m2x4_t (void *base, void *out)
{
  vuint64m2x4_t v = *(vuint64m2x4_t*)base;
  *(vuint64m2x4_t*)out = v;
}

/* { dg-final { scan-assembler-not {srai} } } */
/* { dg-final { scan-assembler {slli} } } */
/* { dg-final { scan-assembler-times {vl2re64\.v\tv[0-9]+,0\([a-x0-9]+\)} 18 } } */
/* { dg-final { scan-assembler-times {vs2r\.v\tv[0-9]+,0\([a-x0-9]+\)} 18 } } */
